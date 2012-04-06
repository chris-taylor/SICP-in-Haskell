-- Modularity, Objects and State

import Control.Monad.State
import Control.Monad.ST
import Data.STRef

type Cash = Float
type Account = State Cash

withdraw :: Cash -> Account (Either String Cash)
withdraw amount = state makewithdrawal where
    makewithdrawal balance = if balance >= amount
        then (Right amount, balance - amount)
        else (Left "Insufficient funds", balance)

deposit :: Cash -> Account ()
deposit amount = state makedeposit where
    makedeposit balance = ((), balance + amount)

{-  Thanks to Daniel Wagner on StackOverflow for explaining the ST monad
    http://stackoverflow.com/questions/10048213/managing-state-chapter-3-of-sicp/10048527#10048527 -}

makeWithdraw :: Cash -> ST s (Cash -> ST s (Either String Cash))
makeWithdraw initialBalance = do
    refBalance <- newSTRef initialBalance
    return $ \amount -> do
        balance <- readSTRef refBalance
        if amount > balance
            then return (Left "Insufficient funds")
            else do
                modifySTRef refBalance (subtract amount)
                return (Right $ balance - amount)

{-  Can test makeWithdraw by running this code: -}

testMakeWithdraw :: [Either String Cash]
testMakeWithdraw = runST $ do
    w1 <- makeWithdraw 100
    w2 <- makeWithdraw 100
    x1 <- w1 50
    y1 <- w2 70
    x2 <- w1 40
    y2 <- w2 40
    return [x1,y1,x2,y2]

