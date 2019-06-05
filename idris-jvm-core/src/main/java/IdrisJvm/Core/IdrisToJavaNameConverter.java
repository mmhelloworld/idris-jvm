package IdrisJvm.Core;

import java.util.HashMap;
import java.util.Map;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

public class IdrisToJavaNameConverter {
    private static final Pattern pattern = Pattern.compile("^([\\p{Alnum}._]*)(.*)$");
    private static final Pattern endsWithNonDotsPattern = Pattern.compile("(.*?)\\.([^.]+)$");
    private static final Pattern endsWithDotsPattern = Pattern.compile("(.*?)\\.(\\.*)$");
    private static final Map<Character, String> replacements = new HashMap<>();
    private static final String DEFAULT_PACKAGE_NAME = "main";
    private static final String DEFAULT_CLASS_NAME = DEFAULT_PACKAGE_NAME + "/Main";
    private static final String DEFAULT_METHOD_NAME = "main";

    static {
        replacements.put(' ', "space");
        replacements.put('!', "excl");
        replacements.put('"', "dquot");
        replacements.put('#', "hash");
        replacements.put('$', "dollar");
        replacements.put('%', "percent");
        replacements.put('&', "amper");
        replacements.put('\'', "squot");
        replacements.put('(', "lpar");
        replacements.put(')', "rpar");
        replacements.put('*', "times");
        replacements.put('+', "plus");
        replacements.put(',', "comma");
        replacements.put('-', "hyphen");
        replacements.put('.', "dot");
        replacements.put('/', "div");
        replacements.put('\\', "bslash");
        replacements.put(':', "colon");
        replacements.put(';', "semicol");
        replacements.put('<', "lt");
        replacements.put('=', "eq");
        replacements.put('>', "gt");
        replacements.put('?', "ques");
        replacements.put('@', "at");
        replacements.put('^', "caret");
        replacements.put('`', "grave");
        replacements.put('{', "lbrace");
        replacements.put('|', "pipe");
        replacements.put('}', "rbrace");
        replacements.put('~', "tilde");
    }

    public static String idrisClassMethodName(String idrisName) {
        final Matcher matcher = pattern.matcher(idrisName);
        if (matcher.find()) {
            final String className = matcher.group(1);
            final String methodName = matcher.group(2);
            final Matcher endsWithNonDotMatcher = endsWithNonDotsPattern.matcher(className);
            if (endsWithNonDotMatcher.find()) {
                String lastPart = endsWithNonDotMatcher.group(2);
                return createClassName(endsWithNonDotMatcher.group(1)) + "," + createMethodName(lastPart + methodName);
            } else {
                final Matcher endsWithDotMatcher = endsWithDotsPattern.matcher(className);
                if (endsWithDotMatcher.find()) {
                    return createClassName(endsWithDotMatcher.group(1)) + "," + createMethodName(endsWithDotMatcher.group(2) + methodName);
                } else {
                    if (methodName.trim().isEmpty()) {
                        return DEFAULT_CLASS_NAME + "," + createMethodName(className);
                    } else {
                        return createClassName(className) + "," + createMethodName(methodName);
                    }
                }
            }
        } else {
            return DEFAULT_CLASS_NAME + "," + DEFAULT_METHOD_NAME;
        }
    }

    private static String createMethodName(final String methodName) {
        StringBuilder builder = new StringBuilder(methodName.length());

        for (char c : methodName.toCharArray()) {
            if (Character.isJavaIdentifierPart(c)) {
                builder.append(c);
            } else if (replacements.containsKey(c)) {
                builder.append("$").append(replacements.get(c));
            } else {
                builder.append((int) c);
            }
        }
        return builder.toString();
    }

    private static String createClassName(final String className) {
        if (className.isEmpty()) {
            return DEFAULT_CLASS_NAME;
        } else if (className.contains(".")) {
            return className.replaceAll("\\.", "/")
                .replaceAll("([^/]+)/", "I\\_$1/");
        } else {
            return DEFAULT_PACKAGE_NAME + "/" + className;
        }
    }
}
