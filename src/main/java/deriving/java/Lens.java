package deriving.java;

import java.lang.reflect.Constructor;
import java.lang.reflect.Parameter;
import java.lang.reflect.Method;
import java.lang.reflect.InvocationTargetException;

public interface Lens<A, B> {

    public B get(A a);

    public A set(A a, B b);

    public static String toCamelCase(String name) {
        return Character.toUpperCase(name.charAt(0)) + name.substring(1);
    }

    public static <A, B> Lens<A, B> lens(Class<A> c, String field) throws NoSuchMethodException {
        Constructor ctor = c.getConstructors()[0];
        Parameter[] params = ctor.getParameters();
        Method method = c.getMethod("get" + toCamelCase(field));
        Method[] methods = new Method[params.length];
        for (int i = 0; i < params.length; i++)
            methods[i] = c.getMethod("get" + toCamelCase(params[i].getName()));
        return new Lens<A, B>() {
            public B get(A a) {
                try {
                    return (B) method.invoke(a);
                } catch (IllegalAccessException | InvocationTargetException e) {
                    throw new RuntimeException(e);
                }
            }
            public A set(A a, B b) {
                try {
                    Object[] args = new Object[methods.length];
                    for (int i = 0; i < methods.length; i++)
                        args[i] = methods[i].equals(method) ? b : methods[i].invoke(a);
                    return (A) ctor.newInstance(args);
                } catch (InstantiationException | IllegalAccessException | InvocationTargetException e) {
                    throw new RuntimeException(e);
                }
            }
        };
    }
    
}
