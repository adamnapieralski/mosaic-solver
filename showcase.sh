#!/bin/bash

cabal build

echo "--------------------------------HEART-------------------------------------"
echo "data/heart.txt" | ./dist/build/mosaic-solver/mosaic-solver
echo "--------------------------------LION--------------------------------------"
echo "data/lion.txt" | ./dist/build/mosaic-solver/mosaic-solver
echo "-----------------------------?SOMETHING?----------------------------------"
echo "data/sth.txt" | ./dist/build/mosaic-solver/mosaic-solver
echo "--------------------------------BOAT--------------------------------------"
echo "data/boat.txt" | ./dist/build/mosaic-solver/mosaic-solver
