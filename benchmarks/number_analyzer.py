import math

def count_digits_recursive(n: int) -> int:
    n = abs(n)
    if n <= 9:
        return 1
    return 1 + count_digits_recursive(n // 10)

def count_digits_iterative(n: int) -> int:
    if n == 0:
        return 1
    n = abs(n)
    count = 0
    while n != 0:
        n //= 10
        count += 1
    return count

def analyze_number(n: int) -> dict[str, int]:
    return {
        "original_number": n,
        "digit_count_recursive": count_digits_recursive(n),
        "digit_count_iterative": count_digits_iterative(n),
        "is_negative": 1 if n < 0 else 0,
        "is_even": 1 if n % 2 == 0 else 0
    }

def test_analyze_positive():
    res = analyze_number(156)
    assert res["original_number"] == 156
    assert res["digit_count_recursive"] == 3
    assert res["digit_count_iterative"] == 3
    assert res["is_negative"] == 0
    assert res["is_even"] == 1

def test_analyze_negative():
    res = analyze_number(-7)
    assert res["original_number"] == -7
    assert res["digit_count_recursive"] == 1
    assert res["digit_count_iterative"] == 1
    assert res["is_negative"] == 1
    assert res["is_even"] == 0

if __name__ == "__main__":
    test_analyze_positive()
    test_analyze_negative()
