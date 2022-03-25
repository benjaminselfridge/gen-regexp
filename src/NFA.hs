{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE TypeFamilies #-}

module NFA where

import Control.Monad (void)
import Control.Monad.ST
import Data.Graph.Haggle
import Data.List (nub)
import Data.MonoTraversable (Element)
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Data.Sequences (IsSequence, snoc)
import Data.Set (Set)
import qualified Data.Set as Set

data NFA sigma = NFA { graph :: EdgeLabeledGraph Digraph sigma
                     , startState :: Vertex
                     , finalStates :: Set Vertex
                     }

-- | One round of string generation.
generate1 :: (Eq string, Ord string, IsSequence string, sigma ~ Element string)
          => EdgeLabeledGraph Digraph sigma
          -- ^ The NFA graph
          -> Set (Vertex, string)
          -- ^ set of discovered (Vertex, String) pairs thus far
          -> Seq (Vertex, string)
          -- ^ (Vertex, String) pairs to generate from
          -> (Set (Vertex, string), Seq (Vertex, string))
          -- ^ The new set (with the newly-discovered pairs), plus the new pairs
          -- that we need to generate from next
generate1 _ seen Seq.Empty = (seen, Seq.Empty)
generate1 g seen ((v, s) Seq.:<| from) =
  let new =  [ (d, s `snoc` l) | e <- outEdges g v
                               , let d = edgeDest e
                               , let Just l = edgeLabel g e ]
  in ( seen `Set.union` Set.fromList new
     , from Seq.>< [ pair | pair <- Seq.fromList new, (not . (`elem` seen)) pair ]
     )

iterateM :: Monad m => Int -> (a -> m a) -> a -> m a
iterateM 0 _ a = return a
iterateM n f a = f a >>= iterateM (n-1) f

generate' :: (Eq string, Ord string, IsSequence string, sigma ~ Element string)
          => EdgeLabeledGraph Digraph sigma
          -- ^ The NFA graph
          -> Set (Vertex, string)
          -- ^ set of discovered (Vertex, String) pairs thus far
          -> Seq (Vertex, string)
          -- ^ (Vertex, String) pairs to generate from
          -> [(Set (Vertex, string), Seq (Vertex, string))]
          -- ^ The new set (with the newly-discovered pairs), plus the new pairs
          -- that we need to generate from next
generate' g seen from = iterate (uncurry (generate1 g)) (seen, from)

-- | Generate strings from an NFA. The 'Int' is the number of times to walk the graph.
generate :: (Eq string, Ord string, IsSequence string, sigma ~ Element string)
         => Int
         -- ^ How many rounds of generation to perform
         -> NFA sigma
         -> [string]
generate n nfa = nub
  [ s | (pairs, _) <- take n $ generate' (graph nfa) Set.empty (Seq.singleton (startState nfa, mempty))
      , (q, s) <- Set.toList pairs
      , q `Set.member` finalStates nfa
      ]

-- | NFA for cs(fs)*
example1 :: NFA Char
example1 = runST $ do
  g <- newEdgeLabeledGraph newMDigraph

  -- The states
  q0 <- addVertex g
  q1 <- addVertex g
  q2 <- addVertex g
  q3 <- addVertex g

  void $ addLabeledEdge g q0 q1 'c'
  void $ addLabeledEdge g q1 q2 's'
  void $ addLabeledEdge g q2 q3 'f'
  void $ addLabeledEdge g q3 q2 's'

  g' <- freeze g
  return $ NFA g' q0 (Set.fromList [q2])
