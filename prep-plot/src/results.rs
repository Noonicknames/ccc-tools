use std::collections::HashMap;

use crate::config::{SingleState, SingleTransition, State, Transition};

#[derive(Debug)]
pub struct CsResults {
    pub cross_sections: HashMap<SingleTransition, Vec<(f64, f64)>>,
    pub to_ion_cross_sections: HashMap<SingleState, Vec<(f64, f64)>>,
}

impl CsResults {
    /// Construct self without any entries.
    pub fn empty() -> Self {
        Self {
            cross_sections: HashMap::new(),
            to_ion_cross_sections: HashMap::new(),
        }
    }
    pub fn contains_key(&self, transition: &Transition) -> bool {
        match transition {
            Transition {
                to: State::Ionized,
                from: State::Single(from),
            } => self.to_ion_cross_sections.contains_key(from),
            Transition {
                to: State::Single(to),
                from: State::Single(from),
            } => self.cross_sections.contains_key(&SingleTransition {
                to: to.clone(),
                from: from.clone(),
            }),
            _ => unimplemented!(),
        }
    }
    pub fn push_single_energy_results(&mut self, results: &SingleEnergyResults) {
        results.cross_sections.iter().for_each(|(transition, cs)| {
            self.push_single_result(transition, (results.energy, *cs));
        });
        results.to_ion_cross_sections.iter().for_each(|(from, cs)| {
            self.push_to_ion_result(from, (results.energy, *cs));
        });
    }
    pub fn push_single_result(&mut self, transition: &SingleTransition, result: (f64, f64)) {
        let cs_vec = match self.cross_sections.get_mut(&transition) {
            Some(cs_vec) => cs_vec,
            None => {
                self.cross_sections.insert(transition.clone(), Vec::new());
                unsafe { self.cross_sections.get_mut(&transition).unwrap_unchecked() }
            }
        };
        cs_vec.push(result);
    }
    pub fn push_to_ion_result(&mut self, from: &SingleState, result: (f64, f64)) {
        let cs_vec = match self.to_ion_cross_sections.get_mut(from) {
            Some(cs_vec) => cs_vec,
            None => {
                self.to_ion_cross_sections.insert(from.clone(), Vec::new());
                unsafe { self.to_ion_cross_sections.get_mut(from).unwrap_unchecked() }
            }
        };
        cs_vec.push(result);
    }
    pub fn push_result(&mut self, transition: &Transition, result: (f64, f64)) {
        match transition {
            Transition {
                to: State::Ionized,
                from: State::Single(from),
            } => {
                self.push_to_ion_result(from, result);
            }
            Transition {
                to: State::Single(to),
                from: State::Single(from),
            } => {
                let transition = SingleTransition {
                    to: to.clone(),
                    from: from.clone(),
                };
                self.push_single_result(&transition, result);
            }
            _ => unimplemented!(),
        }
    }
    pub fn transitions_count(&self) -> usize {
        self.cross_sections.len() + self.to_ion_cross_sections.len()
    }
    pub fn sort(&mut self) {
        self.cross_sections
            .values_mut()
            .for_each(|cs| cs.sort_by(|l, r| l.0.total_cmp(&r.0)));
        self.to_ion_cross_sections
            .values_mut()
            .for_each(|cs| cs.sort_by(|l, r| l.0.total_cmp(&r.0)));
    }
    pub fn iter(&self) -> impl Iterator<Item = (Transition, &Vec<(f64, f64)>)> {
        self.cross_sections
            .iter()
            .map(|(transition, cs_vec)| (transition.clone().into(), cs_vec))
            .chain(self.to_ion_cross_sections.iter().map(|(state, cs_vec)| {
                (
                    Transition {
                        to: State::Ionized,
                        from: State::Single(state.clone()),
                    },
                    cs_vec,
                )
            }))
    }
    pub fn iter_transitions(&self) -> impl Iterator<Item = Transition> + '_ {
        self.cross_sections
            .keys()
            .map(|transition| transition.clone().into())
            .chain(self.to_ion_cross_sections.keys().map(|state| Transition {
                to: State::Ionized,
                from: State::Single(state.clone()),
            }))
    }
    pub fn get(&self, transition: &Transition) -> Option<&Vec<(f64, f64)>> {
        match transition {
            Transition {
                to: State::Ionized,
                from: State::Single(state),
            } => self.to_ion_cross_sections.get(state),
            Transition {
                to: State::Single(to),
                from: State::Single(from),
            } => self.cross_sections.get(&SingleTransition {
                to: to.clone(),
                from: from.clone(),
            }),
            _ => unimplemented!(),
        }
    }
}

/// Since each totalcs file are the results for a single energy, those files should be parsed into this struct.
#[derive(Default)]
pub struct SingleEnergyResults {
    pub energy: f64,
    pub cross_sections: HashMap<SingleTransition, f64>,
    pub to_ion_cross_sections: HashMap<SingleState, f64>,
}
