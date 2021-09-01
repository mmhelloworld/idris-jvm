package io.github.mmhelloworld.idrisjvm.runtime;

import static java.nio.charset.StandardCharsets.UTF_8;

public final class Strings {
    private Strings() {
    }

    public static String substring(int offset, int length, String string) {
        // matches scheme implementation function "string-substr"
        int strLength = string.length();
        int start = Math.max(0, offset);
        int nonNegativeLength = Math.max(0, length);
        int end = Math.min(strLength, start + nonNegativeLength);
        return start > strLength ? "" : string.substring(start, end);
    }

    public static String concat(IdrisList idrisList) {
        StringBuilder builder = new StringBuilder();
        IdrisObject current = (IdrisObject) idrisList;
        while (current.getConstructorId() != 0) {
            builder.append(current.getProperty(0));
            current = (IdrisObject) current.getProperty(1);
        }
        return builder.toString();
    }

    public static String pack(IdrisList idrisCharacterList) {
        Object[] objectArray = idrisCharacterList.toArray();
        char[] chars = new char[objectArray.length];
        for (int index = 0; index < objectArray.length; index++) {
            chars[index] = (char) objectArray[index];
        }
        return String.valueOf(chars);
    }

    public static IdrisList unpack(String string) {
        return IdrisList.fromArray(string.toCharArray());
    }

    public static int bytesLengthUtf8(String string) {
        return string.getBytes(UTF_8).length;
    }

    public static int newStringIterator(String string) {
        return 0;
    }

    public static IdrisObject nextStringIterator(String string, int offset) {
        // matches scheme implementation function blodwen-string-iterator-next
        return offset >= string.length() ? NilUnconsResult.INSTANCE : new CharacterUnconsResult(string, offset);
    }

    private static final class NilUnconsResult implements IdrisObject {
        static final NilUnconsResult INSTANCE = new NilUnconsResult();

        private NilUnconsResult() {
        }

        @Override
        public int getConstructorId() {
            return 0;
        }

        @Override
        public String toString() {
            return "NilUnconsResult";
        }
    }

    private static final class CharacterUnconsResult implements IdrisObject {
        private final char character;
        private final int newOffset;

        CharacterUnconsResult(String string, int offset) {
            this.character = string.charAt(offset);
            this.newOffset = offset + 1;
        }

        @Override
        public int getConstructorId() {
            return 1;
        }

        @Override
        public Object getProperty(int index) {
            switch (index) {
                case 0:
                    return character;
                case 1:
                    return newOffset;
                default:
                    throw new IllegalArgumentException("No property at " + index);
            }
        }

        @Override
        public String toString() {
            return "CharacterUnconsResult{character=" + character + ", newOffset=" + newOffset + "}";
        }
    }
}
