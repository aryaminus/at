import java.util.HashMap;
import java.util.Map;

public class NumberAnalyzer {
    public static int countDigitsRecursive(int n) {
        int num = Math.abs(n);
        if (num <= 9) return 1;
        return 1 + countDigitsRecursive(num / 10);
    }
    
    public static int countDigitsIterative(int n) {
        if (n == 0) return 1;
        int num = Math.abs(n);
        int count = 0;
        while (num != 0) {
            num = num / 10;
            count++;
        }
        return count;
    }
    
    public static Map<String, Integer> analyzeNumber(int n) {
        Map<String, Integer> res = new HashMap<>();
        res.put("original_number", n);
        res.put("digit_count_recursive", countDigitsRecursive(n));
        res.put("digit_count_iterative", countDigitsIterative(n));
        res.put("is_negative", n < 0 ? 1 : 0);
        res.put("is_even", n % 2 == 0 ? 1 : 0);
        return res;
    }

    public static void testAnalyzePositive() {
        Map<String, Integer> res = analyzeNumber(156);
        assert res.get("original_number") == 156;
        assert res.get("digit_count_recursive") == 3;
        assert res.get("digit_count_iterative") == 3;
        assert res.get("is_negative") == 0;
        assert res.get("is_even") == 1;
    }

    public static void testAnalyzeNegative() {
        Map<String, Integer> res = analyzeNumber(-7);
        assert res.get("original_number") == -7;
        assert res.get("digit_count_recursive") == 1;
        assert res.get("digit_count_iterative") == 1;
        assert res.get("is_negative") == 1;
        assert res.get("is_even") == 0;
    }

    public static void main(String[] args) {
        // Run tests
        testAnalyzePositive();
        testAnalyzeNegative();
    }
}
