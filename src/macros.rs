#[macro_export]
macro_rules! is {
    ($value: expr) => {
        $value != 0
    };
}

#[macro_export]
macro_rules! compare {
    ($self:ident, $a: expr, $b: expr) => {{
        let a = $a;
        let b = $b;
        $self.compare(a, b);
    }};
}

#[macro_export]
macro_rules! increment {
    ($self:ident, $value: expr, $amount: expr) => {{
        let result = ($value as Byte).wrapping_add($amount as Byte);
        compare!($self, result, 0);
        result
    }};
}
