use std::{
    future::Future, ops::{Bound, Deref, RangeBounds}, path::{Path, PathBuf}, sync::Arc
};

use futures::{stream::BoxStream, StreamExt};
use serde::{Deserialize, Serialize};
use tokio_stream::wrappers::ReadDirStream;

pub async fn ensure_folder_exists(folder_path: impl AsRef<Path>) -> Result<(), std::io::Error> {
    match tokio::fs::try_exists(folder_path.as_ref()).await? {
        false => tokio::fs::create_dir(folder_path.as_ref()).await?,
        true => (),
    }

    Ok(())
}

/// Source to pull files from.
///
/// Used for configuring how to extract results.
#[derive(Clone, Debug, Serialize, Deserialize, Hash, PartialEq, Eq, Default)]
pub enum FilesSource {
    Folder(String),
    Folders(Vec<String>),
    File(String),
    #[default]
    None,
}

#[derive(thiserror::Error, Debug)]
pub enum FilesError {
    #[error("Error reading an entry in directory {dir_path}: {err}")]
    DirEntry {
        dir_path: PathBuf,
        err: std::io::Error,
    },
    #[error("Error reading type of a directory entry {entry_path}: {err}")]
    DirEntryType {
        entry_path: PathBuf,
        err: std::io::Error,
    },
    #[error("Path `{path_given}` given is not a file")]
    NotAFile { path_given: PathBuf },
}

impl FilesSource {
    /// Get a stream of all files from the given source.
    pub fn files<'a, Fut, F>(
        &self,
        root_dir: impl AsRef<Path>,
        directory_error_handle: F,
    ) -> BoxStream<'a, Result<PathBuf, FilesError>>
    where
        F: Fn(std::io::Error) -> Fut + Send + Sync + 'a,
        Fut: Future<Output = ()> + Send,
    {
        let directory_error_handle = Arc::new(directory_error_handle);
        match self {
            FilesSource::Folder(folder) => {
                // To avoid cloning, use Arc.
                let path = Arc::new(root_dir.as_ref().join(&folder));

                futures::stream::once({
                    let path = Arc::clone(&path);
                    async move { tokio::fs::read_dir(path.as_ref()).await }
                })
                .filter_map(move |dir| {
                    let directory_error_handle = Arc::clone(&directory_error_handle);
                    async move {
                        match dir {
                            Ok(dir) => Some(ReadDirStream::new(dir)),
                            Err(err) => {
                                (directory_error_handle)(err).await;
                                None
                            }
                        }
                    }
                })
                .flatten()
                .filter_map(move |dir_entry| {
                    let path = Arc::clone(&path);
                    async move {
                        match dir_entry {
                            Ok(dir_entry) => {
                                if !dir_entry
                                    .file_type()
                                    .await
                                    .is_ok_and(|file_type| file_type.is_file())
                                {
                                    return None;
                                }

                                Some(Ok(dir_entry.path()))
                            }
                            Err(err) => Some(Err(FilesError::DirEntry {
                                dir_path: path.as_ref().clone(),
                                err: err,
                            })),
                        }
                    }
                })
                .boxed()
            }
            FilesSource::Folders(folders) => {
                let folders = folders
                    .iter()
                    .map(|folder| Arc::new(root_dir.as_ref().join(&folder)))
                    .collect::<Vec<_>>();
                futures::stream::iter(folders)
                    .filter_map(move |path| {
                        let directory_error_handle = Arc::clone(&directory_error_handle);
                        async move {
                            match tokio::fs::read_dir(path.as_ref()).await {
                                Ok(read_dir) => Some((path, ReadDirStream::new(read_dir))),
                                Err(err) => {
                                    (directory_error_handle)(err).await;
                                    None
                                }
                            }
                        }
                    })
                    .flat_map_unordered(None, |(path, read_dir)| {
                        read_dir.map(move |read_dir| (Arc::clone(&path), read_dir))
                    })
                    .filter_map(move |(path, dir_entry)| async move {
                        match dir_entry {
                            Ok(dir_entry) => {
                                if !dir_entry
                                    .file_type()
                                    .await
                                    .is_ok_and(|file_type| file_type.is_file())
                                {
                                    return None;
                                }

                                Some(Ok(dir_entry.path()))
                            }
                            Err(err) => Some(Err(FilesError::DirEntry {
                                dir_path: path.as_ref().clone(),
                                err: err,
                            })),
                        }
                    })
                    .boxed()
            }
            FilesSource::File(file) => {
                let path = root_dir.as_ref().join(&file);
                futures::stream::once(async move { path })
                    .filter_map(|path| async move {
                        let metadata = match tokio::fs::metadata(&path).await {
                            Ok(metadata) => metadata,
                            Err(err) => {
                                return Some(Err(FilesError::DirEntryType {
                                    entry_path: path,
                                    err,
                                }))
                            }
                        };
                        if metadata.is_file() {
                            Some(Ok(path))
                        } else {
                            Some(Err(FilesError::NotAFile { path_given: path }))
                        }
                    })
                    .boxed()
            }
            FilesSource::None => {
                futures::stream::empty().boxed()
            }
        }
    }
}

pub trait ColumnSlice<'a> {
    type Output: 'a;
    fn slice_columns(&'a self, range: impl RangeBounds<usize>) -> Option<Self::Output>;
}

impl<'a, T> ColumnSlice<'a> for T
where
    T: 'a + Deref<Target = str>,
{
    type Output = &'a str;
    fn slice_columns(&'a self, range: impl RangeBounds<usize>) -> Option<Self::Output> {
        get_column_slice(self.deref(), range)
    }
}

pub fn get_column_slice(line: &str, range: impl RangeBounds<usize>) -> Option<&str> {
    let end = match range.end_bound() {
        Bound::Excluded(end) => Some(end.checked_sub(1)?),
        Bound::Included(end) => Some(*end),
        Bound::Unbounded => None,
    };

    let start = match range.start_bound() {
        Bound::Excluded(start) => start + 1,
        Bound::Included(start) => *start,
        Bound::Unbounded => 0,
    };

    if end.map(|end| end < start).unwrap_or(false) {
        return None;
    }

    let mut column_num = 0;
    let mut chars = line.char_indices();
    let mut start_idx = None;
    let mut end_idx = None;

    'outer: loop {
        // In whitespace
        'inner: loop {
            let Some((idx, char)) = chars.next() else {
                break 'outer;
            };

            if !char.is_whitespace() {
                if column_num == start {
                    start_idx = Some(idx);
                }
                break 'inner;
            }
        }

        // Not in whitespace
        'inner: loop {
            let Some((idx, char)) = chars.next() else {
                if end.map(|end| end == column_num).unwrap_or(true) {
                    end_idx = Some(line.len());
                }
                break 'outer;
            };

            if char.is_whitespace() {
                match end {
                    Some(end) if end == column_num => {
                        end_idx = Some(idx);
                        break 'outer;
                    }
                    None => {
                        end_idx = Some(idx);
                    }
                    _ => {}
                }
                column_num += 1;
                break 'inner;
            }
        }
    }

    if let (Some(start_idx), Some(end_idx)) = (start_idx, end_idx) {
        Some(&line[start_idx..end_idx])
    } else {
        None
    }
}

#[cfg(test)]
mod tests {
    use crate::util::get_column_slice;

    #[test]
    fn test_get_column_slice() {
        let lines = [
            "   one two three four five six seven eight nine ten             ",
            "one two three four five six seven eight nine ten      ",
            "   one two three four five six seven eight nine ten",
            "one two three four five six seven eight nine ten",
        ];
        for line in lines {
            for i in 0..20 {
                assert_eq!(get_column_slice(line, i..i), None);
            }
        }
        for line in lines {
            for i in 0..20 {
                assert_eq!(
                    get_column_slice(line, i..),
                    get_column_slice(line, i..10),
                    "{}.. == {}..10",
                    i,
                    i
                );
            }
        }
        for line in lines {
            for i in 0..20 {
                assert_eq!(
                    get_column_slice(line, ..i),
                    get_column_slice(line, 0..i),
                    "..{} == 0..{}",
                    i,
                    i
                );
            }
        }
        for line in lines {
            assert_eq!(get_column_slice(line, 0..=0), Some("one"));
            assert_eq!(get_column_slice(line, 1..=1), Some("two"));
            assert_eq!(get_column_slice(line, 2..=2), Some("three"));
            assert_eq!(get_column_slice(line, 3..=3), Some("four"));
            assert_eq!(get_column_slice(line, 4..=4), Some("five"));
            assert_eq!(get_column_slice(line, 5..=5), Some("six"));
            assert_eq!(get_column_slice(line, 6..=6), Some("seven"));
            assert_eq!(get_column_slice(line, 7..=7), Some("eight"));
            assert_eq!(get_column_slice(line, 8..=8), Some("nine"));
            assert_eq!(get_column_slice(line, 9..=9), Some("ten"));
        }
        for line in lines {
            assert_eq!(get_column_slice(line, 0..1), Some("one"));
            assert_eq!(get_column_slice(line, 1..2), Some("two"));
            assert_eq!(get_column_slice(line, 2..3), Some("three"));
            assert_eq!(get_column_slice(line, 3..4), Some("four"));
            assert_eq!(get_column_slice(line, 4..5), Some("five"));
            assert_eq!(get_column_slice(line, 5..6), Some("six"));
            assert_eq!(get_column_slice(line, 6..7), Some("seven"));
            assert_eq!(get_column_slice(line, 7..8), Some("eight"));
            assert_eq!(get_column_slice(line, 8..9), Some("nine"));
            assert_eq!(get_column_slice(line, 9..10), Some("ten"));
        }
        for line in lines {
            assert_eq!(get_column_slice(line, 0..2), Some("one two"));
            assert_eq!(get_column_slice(line, 1..3), Some("two three"));
            assert_eq!(get_column_slice(line, 2..4), Some("three four"));
            assert_eq!(get_column_slice(line, 3..5), Some("four five"));
            assert_eq!(get_column_slice(line, 4..6), Some("five six"));
            assert_eq!(get_column_slice(line, 5..7), Some("six seven"));
            assert_eq!(get_column_slice(line, 6..8), Some("seven eight"));
            assert_eq!(get_column_slice(line, 7..9), Some("eight nine"));
            assert_eq!(get_column_slice(line, 8..10), Some("nine ten"));
        }
        for line in lines {
            assert_eq!(get_column_slice(line, 0..3), Some("one two three"));
            assert_eq!(get_column_slice(line, 1..4), Some("two three four"));
            assert_eq!(get_column_slice(line, 2..5), Some("three four five"));
            assert_eq!(get_column_slice(line, 3..6), Some("four five six"));
            assert_eq!(get_column_slice(line, 4..7), Some("five six seven"));
            assert_eq!(get_column_slice(line, 5..8), Some("six seven eight"));
            assert_eq!(get_column_slice(line, 6..9), Some("seven eight nine"));
            assert_eq!(get_column_slice(line, 7..10), Some("eight nine ten"));
        }
        for line in lines {
            assert_eq!(get_column_slice(line, 0..4), Some("one two three four"));
            assert_eq!(get_column_slice(line, 1..5), Some("two three four five"));
            assert_eq!(get_column_slice(line, 2..6), Some("three four five six"));
            assert_eq!(get_column_slice(line, 3..7), Some("four five six seven"));
            assert_eq!(get_column_slice(line, 4..8), Some("five six seven eight"));
            assert_eq!(get_column_slice(line, 5..9), Some("six seven eight nine"));
            assert_eq!(get_column_slice(line, 6..10), Some("seven eight nine ten"));
        }
        for line in lines {
            assert_eq!(
                get_column_slice(line, 0..5),
                Some("one two three four five")
            );
            assert_eq!(
                get_column_slice(line, 1..6),
                Some("two three four five six")
            );
            assert_eq!(
                get_column_slice(line, 2..7),
                Some("three four five six seven")
            );
            assert_eq!(
                get_column_slice(line, 3..8),
                Some("four five six seven eight")
            );
            assert_eq!(
                get_column_slice(line, 4..9),
                Some("five six seven eight nine")
            );
            assert_eq!(
                get_column_slice(line, 5..10),
                Some("six seven eight nine ten")
            );
        }
        for line in lines {
            assert_eq!(
                get_column_slice(line, 0..6),
                Some("one two three four five six")
            );
            assert_eq!(
                get_column_slice(line, 1..7),
                Some("two three four five six seven")
            );
            assert_eq!(
                get_column_slice(line, 2..8),
                Some("three four five six seven eight")
            );
            assert_eq!(
                get_column_slice(line, 3..9),
                Some("four five six seven eight nine")
            );
            assert_eq!(
                get_column_slice(line, 4..10),
                Some("five six seven eight nine ten")
            );
        }
        for line in lines {
            assert_eq!(
                get_column_slice(line, 0..7),
                Some("one two three four five six seven")
            );
            assert_eq!(
                get_column_slice(line, 1..8),
                Some("two three four five six seven eight")
            );
            assert_eq!(
                get_column_slice(line, 2..9),
                Some("three four five six seven eight nine")
            );
            assert_eq!(
                get_column_slice(line, 3..10),
                Some("four five six seven eight nine ten")
            );
        }
        for line in lines {
            assert_eq!(
                get_column_slice(line, 0..8),
                Some("one two three four five six seven eight")
            );
            assert_eq!(
                get_column_slice(line, 1..9),
                Some("two three four five six seven eight nine")
            );
            assert_eq!(
                get_column_slice(line, 2..10),
                Some("three four five six seven eight nine ten")
            );
        }
        for line in lines {
            assert_eq!(
                get_column_slice(line, 0..9),
                Some("one two three four five six seven eight nine")
            );
            assert_eq!(
                get_column_slice(line, 1..10),
                Some("two three four five six seven eight nine ten")
            );
        }
    }
}
