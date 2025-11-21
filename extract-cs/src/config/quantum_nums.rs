/// Principle number corresponding to the energy of a target state with a particular symmetry.
#[derive(Clone, Copy, Default, Hash, PartialEq, Eq, Debug)]
pub struct PrincipleNum(pub u16);

impl PrincipleNum {
    const WRAP_AROUND: u16 = (b'z' - b'0' + 1) as u16;

    /// The ascii byte that would be used to represent this principle number.
    ///
    /// This byte will wrap around if the principle number is large enough and the overflow will be represented by its 'order', see [`Self::order`].
    pub fn as_byte_char(&self) -> u8 {
        (self.0 % Self::WRAP_AROUND) as u8 + b'0'
    }

    /// Get the principle number from the ascii character which represents it.
    ///
    /// Ascii characters may overflow so the 'order' of this overflow may need to be taken into account.
    pub fn from_byte_char(byte: u8, order: u16) -> Option<Self> {
        match byte {
            b'0'..=b'z' => (),
            _ => return None,
        }
        Some(Self((byte - b'0') as u16 + order * Self::WRAP_AROUND))
    }

    pub fn set_order(&mut self, order: u16) {
        self.0 = self.0 % Self::WRAP_AROUND + Self::WRAP_AROUND * order;
    }

    /// Returns the order of the n number.
    ///
    /// Order refers to how many times the display byte for this number has wrapped around.
    pub fn order(&self) -> u16 {
        self.0 / Self::WRAP_AROUND
    }
}

#[derive(Clone, Copy, Default, Hash, PartialEq, Eq, Debug)]
pub enum SpinNum {
    #[default]
    S,
    T,
}

impl SpinNum {
    /// The ascii byte that would be used to represent this spin number.
    pub fn as_byte_char(&self, is_natural_parity: bool) -> u8 {
        match (self, is_natural_parity) {
            (Self::S, true) => b's',
            (Self::S, false) => b'S',
            (Self::T, true) => b't',
            (Self::T, false) => b'T',
        }
    }

    /// Get the spin number and whether the parity is natural from a given byte character.
    pub fn from_byte_char(byte: u8) -> Option<(Self, bool)> {
        Some(match byte {
            b's' => (Self::S, true),
            b'S' => (Self::S, false),
            b't' => (Self::T, true),
            b'T' => (Self::T, false),
            _ => return None,
        })
    }
}

#[derive(Clone, Copy, Default, Hash, PartialEq, Eq, Debug)]
pub enum AngularMomentumNum {
    #[default]
    S,
    P,
    D,
    F,
    G,
    H,
    I,
    J,
    K,
    L,
    M,
    N,
    O,
}

impl AngularMomentumNum {
    /// The ascii byte that would be used to represent this spin number.
    pub fn as_byte_char(&self) -> u8 {
        match self {
            Self::S => b'S',
            Self::P => b'P',
            Self::D => b'D',
            Self::F => b'F',
            Self::G => b'G',
            Self::H => b'H',
            Self::I => b'I',
            Self::J => b'J',
            Self::K => b'K',
            Self::L => b'L',
            Self::M => b'M',
            Self::N => b'N',
            Self::O => b'O',
        }
    }
    /// The ascii byte that would be used to represent this spin number.
    pub fn from_byte_char(byte: u8) -> Option<Self> {
        Some(match byte {
            b'S' => Self::S,
            b'P' => Self::P,
            b'D' => Self::D,
            b'F' => Self::F,
            b'G' => Self::G,
            b'H' => Self::H,
            b'I' => Self::I,
            b'J' => Self::J,
            b'K' => Self::K,
            b'L' => Self::L,
            b'M' => Self::M,
            b'N' => Self::N,
            b'O' => Self::O,
            _ => return None,
        })
    }
}
