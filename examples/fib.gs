fn fib_recursion(n: int) int {
    if n <= 1 {
    	1
    } else {
        fib_recursion(n - 1) + fib_recursion(n - 2)
    }
}

fn fib_loop(n: int) int {
    i = 0;
    n0 = 1;
    n1 = 1;
    loop {
        if i > 0 {
        	temp = n1;
        	n1 = n1 + n0;
        	n0 = temp;
        }
        i = i + 1;
        if i == n {
        	break
        }
    }
    n1
}
