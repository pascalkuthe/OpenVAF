macro_rules! warn {
    ($($arg:tt)*) => {{
        use ::termcolor::{Color, ColorChoice, ColorSpec, StandardStream, WriteColor};
        use ::std::io::Write;
        let mut stderr = StandardStream::stderr(ColorChoice::Auto);
        stderr.set_color(ColorSpec::new().set_fg(Some(Color::Yellow))).unwrap();
        write!(&mut stderr, "warning: ").unwrap();
        stderr.set_color(&ColorSpec::new()).unwrap();
        writeln!(&mut stderr, $($arg)*).unwrap();
    }};
}
