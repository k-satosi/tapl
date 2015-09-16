import Control.Applicative

data Term = TmTrue
          | TmFalse
          | TmIf Term Term Term
          | TmZero
          | TmSucc Term
          | TmPred Term
          | TmIsZero Term
          deriving (Eq, Show)

isnumericval :: Term -> Bool
isnumericval TmZero = True
isnumericval (TmSucc t) = isnumericval t
isnumericval _ = False

isval:: Term -> Bool
isval TmTrue = True
isval TmFalse = True
isval t | isnumericval t = True
isval _ = False

eval1 :: Term -> Maybe Term
eval1 (TmIf TmTrue t2 _) = Just t2
eval1 (TmIf TmFalse _ t3) = Just t3
eval1 (TmIf t1 t2 t3) = TmIf <$> t1' <*> (Just t2) <*> (Just t3) where t1' = eval1 t1
eval1 (TmSucc t1) = TmSucc <$> eval1 t1
eval1 (TmPred TmZero) = Just TmZero
eval1 (TmPred (TmSucc nv1)) | isnumericval nv1 = Just nv1
eval1 (TmPred t1) = TmPred <$> eval1 t1
eval1 (TmIsZero TmZero) = Just TmTrue
eval1 (TmIsZero (TmSucc t)) | isnumericval t = Just TmFalse
eval1 (TmIsZero t1) = TmIsZero <$> eval1 t1
eval1 _ = Nothing

eval :: Term -> Maybe Term
eval t = case eval1 t of
              Nothing -> Just t
              Just t' -> eval t'
