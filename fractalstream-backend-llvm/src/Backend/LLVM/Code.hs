{-# language RecursiveDo, OverloadedStrings #-}
module Backend.LLVM.Code
  ( compile
  ) where

import Backend.LLVM.Operand
import Backend.LLVM.Value

import qualified LLVM.AST as AST
import qualified LLVM.AST.Typed as AST
import LLVM.IRBuilder.Module
import LLVM.AST.Operand hiding (local)
import LLVM.IRBuilder.Instruction
import LLVM.IRBuilder.Monad

import Control.Monad.Except
import Control.Monad.Reader

import Language.Type
import Language.Code
import Data.Indexed.Functor
import Fcf (Exp, Eval)

toParameterList :: EnvironmentProxy env -> [(AST.Type, ParameterName)]
toParameterList = \case
  EmptyEnvProxy -> []
  BindingProxy _ t env ->
    (toLLVMType t, NoParameterName) : toParameterList env

compile :: forall env t
         . (KnownEnvironment env, KnownType t)
        => Code '[] env t
        -> Either String AST.Module
compile code = runExcept $
  buildModuleT "compiled code" $ do
    function "kernel"
             (toParameterList (envProxy (Proxy @env)))
             (toLLVMType (typeProxy @t)) $ \rawArgs -> do
      args <- allocaOperands (envProxy (Proxy @env)) rawArgs
      rv <- runReaderT (compileCode code) args
      case typeProxy @t of
        VoidProxy -> retVoid
        retTy     -> ret =<< detypeOperand retTy rv

compileCode :: forall m env t
             . (MonadModuleBuilder m, MonadIRBuilder m, MonadError String m, MonadFix m)
            => Code '[] env t
            -> ReaderT (Context Operand_ env) m (Op t)
compileCode = indexedFold @(CtxOp m) @(Fix (CodeF '[] Value_)) @(CodeF '[] Value_) $ \case

  Block _ body end -> sequence_ body >> end

  Pure _ v -> value_ v

  NoOp -> pure VoidOp

  Set pf _ v -> do
    x <- value_ v
    ctx <- ask
    storeOperand x (withKnownType (typeOfValue v) (getBinding ctx pf))
    pure VoidOp

  SetBind pf _ t body -> do
    x <- body
    ctx <- ask
    storeOperand x (withKnownType t (getBinding ctx pf))
    pure VoidOp

  Let pf name t v _ body -> do
    x <- value_ v
    ptr <- alloca (toLLVMType t) Nothing 0
    storeOperand x ptr
    recallIsAbsent (absentInTail pf) $ do
      ctx <- Bind name t ptr <$> ask
      lift (runReaderT body ctx)

  LetBind pf name t val _ body -> do
    x <- val
    ptr <- alloca (toLLVMType t) Nothing 0
    storeOperand x ptr
    recallIsAbsent (absentInTail pf) $ do
      ctx <- Bind name t ptr <$> ask
      lift (runReaderT body ctx)

  IfThenElse VoidProxy cond yes no -> mdo
    c <- value_ cond >>= detypeOperand BooleanProxy
    condBr c yesLabel noLabel

    yesLabel <- block
    void yes
    br nextLabel

    noLabel <- block
    void no
    br nextLabel

    nextLabel <- block
    pure VoidOp

  IfThenElse t cond yes no -> mdo
    ptr <- alloca (toLLVMType t) Nothing 0
    c <- value_ cond >>= detypeOperand BooleanProxy
    condBr c yesLabel noLabel

    yesLabel <- block
    yesResult <- yes
    storeOperand yesResult ptr
    br nextLabel

    noLabel <- block
    noResult <- no
    storeOperand noResult ptr
    br nextLabel

    nextLabel <- block
    derefOperand t ptr

  DoWhile body -> mdo
    br entry

    entry <- block
    test <- body >>= detypeOperand BooleanProxy
    condBr test entry exit

    exit <- block
    pure VoidOp

  _ -> error "TODO: unhandled Code constructor"

data CtxOp :: (* -> *) -> (Environment,Type) -> Exp *
type instance Eval (CtxOp m et) =
  ReaderT (Context Operand_ (Env et)) m (Op (Ty et))

allocaOperands :: (MonadModuleBuilder m, MonadIRBuilder m, MonadError String m)
               => EnvironmentProxy env
               -> [Operand]
               -> m (Context Operand_ env)
allocaOperands EmptyEnvProxy [] = pure EmptyContext
allocaOperands (BindingProxy name ty env) (op:ops) =
  Bind name ty <$> allocaOperand ty op
               <*> allocaOperands env ops
allocaOperands _ _ =
  throwError "internal error: mismatched environment/args counts"

allocaOperand :: (MonadModuleBuilder m, MonadIRBuilder m, MonadError String m)
              => ScalarProxy t
              -> Operand
              -> m Operand
allocaOperand t op = do
  opTy <- either throwError pure =<< AST.typeOf op
  if opTy /= toLLVMType t
  then throwError "internal error: mismatched operand type"
  else do
    ptr <- alloca opTy Nothing 0
    store ptr 0 op
    pure ptr
