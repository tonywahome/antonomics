def collatz(n):
    while n != 1:
        print(n, end=' ')  # Print the current value of n
        if n % 2 == 0:
            n = n // 2     # If n is even, divide by 2
        else:
            n = 3 * n + 1  # If n is odd, multiply by 3 and add 1
    print(1)  # Print the final 1, which is the stopping point

# Test the function with a starting value of 10
collatz(7)# pretty cool huh!!

