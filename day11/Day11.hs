{-# LANGUAGE BlockArguments #-}

import           Control.Monad
import           Control.Monad.ST
import           Data.Foldable
import           Data.STRef
import qualified Data.Vector                 as V
import qualified Data.Vector.Unboxed         as VU
import qualified Data.Vector.Unboxed.Mutable as VUM

sim100 :: V.Vector (VU.Vector Int) -> Int
sim100 energies = runST do
    es <- traverse VU.thaw energies
    flashes <- newSTRef 0
    let go i j x = when (x > 9) do
            modifySTRef' flashes succ
            VUM.write (es V.! i) j 0
            for_ [pred i..succ i] \i' ->
                for_ (es V.!? i') \r ->
                    for_ [pred j..succ j] \j' ->
                        when (0 <= j' && j' < VUM.length r) do
                            y <- VUM.read r j'
                            when (y > 0) do
                                let y' = succ y
                                VUM.write r j' y'
                                go i' j' y'
    replicateM_ 100 do
        for_ es \r -> VUM.iforM_ r \j -> VUM.write r j . succ
        V.iforM_ es $ VUM.imapM_ . go
    readSTRef flashes

simSync :: V.Vector (VU.Vector Int) -> Int
simSync energies = runST do
    es <- traverse VU.thaw energies
    let run step = do
            sync <- and <$> traverse (VUM.foldl (\b x -> b && (x == 0)) True) es
            if sync then pure step else do
                for_ es \r -> VUM.iforM_ r \j -> VUM.write r j . succ
                V.iforM_ es $ VUM.imapM_ . go
                run $ succ step
        go i j x = when (x > 9) do
            VUM.write (es V.! i) j 0
            for_ [pred i..succ i] \i' ->
                for_ (es V.!? i') \r ->
                    for_ [pred j..succ j] \j' ->
                        when (0 <= j' && j' < VUM.length r) do
                            y <- VUM.read r j'
                            when (y > 0) do
                                let y' = succ y
                                VUM.write r j' y'
                                go i' j' y'
    run 0

main = do
    energies <- V.fromList . map (VU.fromList . map (read . pure)) . lines <$>
        readFile "input.txt"
    print $ sim100 energies
    print $ simSync energies
