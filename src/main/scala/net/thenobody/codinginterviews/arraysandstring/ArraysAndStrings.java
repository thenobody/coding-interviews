package net.thenobody.codinginterviews.arraysandstring;

import java.util.Random;

/**
 * Created by antonvanco on 03/02/2016.
 */
public class ArraysAndStrings {
    static private Random random = new Random();

    public static void escapeSpaces(char[] input, int length) {
        int offset = input.length - length;
        int spaceOffset = input.length - 1;
        for (int i = 0; i < length && spaceOffset > 1; i++) {
            int c = input.length - offset - i - 1;
            if (input[c] == ' ') {
                input[spaceOffset-2] = '%';
                input[spaceOffset-1] = '2';
                input[spaceOffset] = '0';
                spaceOffset -= 3;
            } else {
                input[spaceOffset] = input[c];
                input[c] = ' ';
                spaceOffset--;
            }
        }
    }

    public static String compressString(String input) {
        if (input.length() < 3) {
            return input;
        }

        char[] chars = input.toCharArray();
        int[] counts = new int[chars.length];

        int i = 0;
        char currentChar = chars[i];
        int currentCharIndex = 0;
        while (i < chars.length) {
            if (currentChar != chars[i]) {
                currentChar = chars[i];
                currentCharIndex++;
            }
            counts[currentCharIndex]++;
            i++;
        }

        StringBuilder resultBuilder = new StringBuilder();

        i = 0;
        currentCharIndex = 0;
        while (i < chars.length) {
            resultBuilder.append(chars[i]);
            resultBuilder.append(counts[currentCharIndex]);

            i += counts[currentCharIndex];
            currentCharIndex++;
        }

        String result = resultBuilder.toString();

        return result.length() >= input.length() ? input : result;
    }

    public static void rotate90Degrees(int[][] image, int size) {
        for (int i = 0; i < size; i++) {
            for (int j = i; j < size; j++) {
                if (i != j ) {
                    int buf = image[i][j];
                    image[i][j] = image[j][i];
                    image[j][i] = buf;
                }
            }
        }
    }

    private static void printMatrix(int[][] matrix, int height, int width) {
        StringBuilder sb = new StringBuilder();
        for (int i = 0; i < height; i++) {
            for (int j = 0; j < width; j++) {
                sb.append(matrix[i][j])
                  .append(" ");
            }
            sb.append("\n");
        }
        System.out.println(sb.toString());
    }

    public static boolean isRotation(String string1, String string2) {
        return (string2 + string2).contains(string1);
    }

    public static void main(String[] args) {
        char[] input = "Mr John Smith    ".toCharArray();
        escapeSpaces(input, 13);
        System.out.println("#" + new String(input) + "#");

        System.out.println();
        System.out.println(compressString("aabcccccaaa"));
        System.out.println(compressString("aa"));
        System.out.println(compressString("abc"));
        System.out.println(compressString("aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaabcccccaaa"));

        int[][] image = {
            { 1, 2 ,3 ,4 },
            { 5, 6 ,7 ,8 },
            { 9, 10 ,11 ,12 },
            { 13, 14, 15 ,16 },
        };
        printMatrix(image, 4, 4);
        rotate90Degrees(image, 4);
        printMatrix(image, 4, 4);

        String string1 = "erbottlewat";
        String string2 = "waterbottle";
        System.out.println("string1: " + string1);
        System.out.println("string2: " + string2);
        System.out.println("is rotation: " + isRotation(string1, string2));
        string1 = "something";
        string2 = "notarotation";
        System.out.println("string1: " + string1);
        System.out.println("string2: " + string2);
        System.out.println("is rotation: " + isRotation(string1, string2));
    }
}
