use std::path::Path;


pub async fn ensure_folder_exists(folder_path: impl AsRef<Path>) -> Result<(), std::io::Error> {
    match tokio::fs::try_exists(folder_path.as_ref()).await? {
        false => tokio::fs::create_dir(folder_path.as_ref()).await?,
        true => (),
    }

    Ok(())
}
