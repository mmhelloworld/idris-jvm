module Main

-- Recursive higher-order function with a declared primitive callback type:
-- exercises spec discovery, the typed callback interface, typed apply in
-- the spec body, and the $idrisTailRec reassignment of the callback param.
applyN : Int -> (Int -> Int) -> Int -> Int
applyN 0 f x = x
applyN n f x = applyN (n - 1) f (f x)

-- Non-recursive HOF: the callback is applied twice inside the body.
twice : (Int -> Int) -> Int -> Int
twice f x = f (f x)

-- Polymorphic HOF: out of declaration-driven scope, used to verify a
-- typed callback flows back into a natural Function slot through the
-- interface's default bridge method.
%noinline
applyPoly : (a -> b) -> a -> b
applyPoly f x = f x

-- Escape test: f is a typed callback inside hof's spec, but it is also
-- passed to applyPoly, which expects a plain Function.
hof : (Int -> Int) -> Int -> Int
hof f x = applyPoly f (f x)

-- Tail call passing a FRESH literal lambda: the lambda in the
-- $idrisTailRec reassignment must be created against the typed interface.
countdown : Int -> (Int -> Int) -> Int
countdown 0 f = f 0
countdown n f = countdown (n - 1) (\x => f (x + 1))

-- Returns a function value: the under-applied call `mkAdder 2` is
-- eta-expanded by the compiler into a literal lambda, so it routes
-- through the typed path like any other lambda argument.
%noinline
mkAdder : Int -> (Int -> Int)
mkAdder k = \x => x + k

-- Bridge test: the typed callback is stored in a constructor slot
-- (Object), extracted as a plain function value and applied through the
-- boxed Function.apply path — which dispatches the typed interface's
-- default bridge method at runtime.
%noinline
fromBox : Maybe (Int -> Int) -> Int -> Int
fromBox (Just f) x = f x
fromBox Nothing x = x

boxed : (Int -> Int) -> Int -> Int
boxed f x = fromBox (Just f) x

-- Rank-2 callback slot: the erased `forall a` binder survives as a
-- runtime application stage (the coercion lambda takes the erased
-- argument first and returns the real predicate), so this slot must NOT
-- be classified as an arity-1 callback — doing so emits a primitive
-- cast on a function value (VerifyError, found in the stage-2 self-host
-- soak via TTImp.ProcessData.shaped).
hasLen : Nat -> List a -> Bool
hasLen n xs = length xs == n

%noinline
shapedR : (forall a . List a -> Bool) -> List (List Int) -> Maybe (List Int)
shapedR p [] = Nothing
shapedR p (c :: cs) = if p c then Just c else shapedR p cs

main : IO ()
main = do
  printLn (applyN 10 (\x => x + x) 1)
  printLn (applyN 3 (\x => x * 3) 2)
  printLn (applyN 3 (mkAdder 2) 10)
  printLn (twice (\x => x + 5) 1)
  printLn (hof (\x => x * 2) 3)
  printLn (countdown 3 (\x => x))
  printLn (boxed (\x => x * 10) 4)
  printLn (shapedR (hasLen 2) [[1], [2, 3]])
