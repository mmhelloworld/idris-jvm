module Java.Util.Function

import Java.Lang
import System.FFI

namespace Function

    public export %inline
    Function : Type -> Type -> Type
    Function a b = (Struct "java/util/function/Function apply" [("<>", a), ("<>", b)], Object -> PrimIO Object)

    %foreign "jvm:.apply"
    prim_apply : Function a b -> a -> PrimIO b

    public export %inline
    apply : HasIO io => Function a b -> a -> io b
    apply f a = primIO $ prim_apply f a

namespace BiFunction

    public export %inline
    BiFunction : Type -> Type -> Type -> Type
    BiFunction a b c = (Struct "java/util/function/BiFunction apply" [("<>", a), ("<>", b), ("<>", c)], Object -> Object -> PrimIO Object)

    %foreign "jvm:.apply"
    prim_apply : BiFunction a b c -> a -> b -> PrimIO c

    public export %inline
    apply : HasIO io => {a, b, c: Type} -> (a -> b -> PrimIO c) -> (x: a) -> (y: b) -> io c
    apply {a} {b} {c} f x y = primIO $ prim_apply (jlambda f) x y

namespace ToIntFunction

    public export %inline
    ToIntFunction : Type -> Type
    ToIntFunction a = (Struct "java/util/function/ToIntFunction applyAsInt" [("<>", a)], Object -> PrimIO Int)

    %foreign "jvm:.applyAsInt"
    prim_applyAsInt : ToIntFunction a -> a -> PrimIO Int

    public export %inline
    applyAsInt : HasIO io => ToIntFunction a -> a -> io Int
    applyAsInt f a = primIO $ prim_applyAsInt f a

namespace Predicate

    public export %inline
    Predicate : Type -> Type
    Predicate a = (Struct "java/util/function/Predicate test" [("<>", a)], Object -> PrimIO Bool)

    %foreign "jvm:.test"
    prim_test : Predicate a -> a -> PrimIO Bool

    public export
    test : HasIO io => Predicate a -> a -> io Bool
    test f a = primIO $ prim_test f a

namespace BiPredicate

    public export %inline
    BiPredicate : Type -> Type -> Type
    BiPredicate a b = (Struct "java/util/function/BiPredicate test" [("<>", a), ("<>", b)], Object -> Object -> PrimIO Bool)

    %foreign "jvm:.test"
    prim_test : BiPredicate a b -> a -> b -> PrimIO Bool

    public export %inline
    test : HasIO io => BiPredicate a b -> a -> b -> io Bool
    test f a b = primIO $ prim_test f a b

namespace Supplier

    public export %inline
    Supplier : Type -> Type
    Supplier a = (Struct "java/util/function/Supplier get" [("<>", a)], PrimIO Object)

    %foreign "jvm:.get"
    prim_get : Supplier a -> PrimIO a

    public export %inline
    get : HasIO io => Supplier a -> io a
    get = primIO . prim_get
