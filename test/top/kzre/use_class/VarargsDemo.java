package top.kzre.use_class;

public class VarargsDemo {
    public VarargsDemo(){}

    /**
     * 将前缀与所有整数的和拼接成字符串。
     * @param prefix  前缀字符串
     * @param numbers 可变数量的整数
     * @return 例如 prefix + sum
     */
    public String sum(String prefix, int... numbers) {
        int total = 0;
        for (int n : numbers) total += n;
        return prefix + total;
    }
}