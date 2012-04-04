    {-# LANGUAGE FlexibleInstances, UndecidableInstances, OverlappingInstances #-}

    class A a where
        fa :: a x -> x
        ca :: x -> a x

    data AImpl x = AImpl x deriving (Eq,Show)

    instance A AImpl where
        fa (AImpl x) = x
        ca x = AImpl x

    instance (A a, Num x, Show (a x), Eq (a x)) => Num (a x) where
        a1 + a2 = ca (fa a1 + fa a2)
        -- other implementations go here...


    class B b where
        fb :: b x -> x
        cb :: x -> b x
        
    data BImpl x = BImpl x

    instance B BImpl where
        fb (BImpl x) = x
        cb x = BImpl x

    instance (B b, Num x, Show (b x), Eq (b x)) => Num (b x) where
        -- implementations go here