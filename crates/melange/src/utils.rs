use num_complex::Complex64;
use pretty_dtoa::{dtoa, FmtFloatConfig};

pub(crate) trait PrettyPrint: Copy {
    fn pretty_str(self) -> String;
}

impl PrettyPrint for f64 {
    fn pretty_str(self) -> String {
        let config =
            FmtFloatConfig::default().add_point_zero(false).max_significant_digits(4).round();
        dtoa(self, config)
    }
}

impl PrettyPrint for Complex64 {
    fn pretty_str(self) -> String {
        let mut text = self.re.pretty_str();

        if self.im != 0.0 {
            if self.im.is_sign_negative() {
                text.push_str(" - j ");
                text.push_str(&(-self.im).pretty_str());
            } else {
                text.push_str(" + j ");
                text.push_str(&(self.im).pretty_str());
            }
        }

        text
    }
}
