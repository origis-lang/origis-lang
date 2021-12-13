#[main]
fn this_main() {
    sum(1, 2); // 6
}

@add(3)
fn sum(a, b: number): number {
    a + b
}

