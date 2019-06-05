package idrisjvm.integrationtest;

import java.lang.annotation.Retention;
import java.lang.annotation.Target;
import java.util.concurrent.TimeUnit;

import static java.lang.annotation.ElementType.METHOD;
import static java.lang.annotation.ElementType.PARAMETER;
import static java.lang.annotation.RetentionPolicy.RUNTIME;

@Target(value = {METHOD, PARAMETER})
@Retention(value = RUNTIME)
public @interface TestAnn2 {
    byte byteValue() default 0;

    boolean booleanValue() default false;

    char charValue() default '\0';

    short shortValue() default 0;

    int intValue() default 0;

    long longValue() default 0;

    float floatValue() default 0;

    double doubleValue() default 0;

    Class<?> classValue() default Void.class;

    TimeUnit enumValue() default TimeUnit.SECONDS;
}
