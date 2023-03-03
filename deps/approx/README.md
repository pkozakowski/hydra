# approx

## Motivation

The library is created to allow for a easy-to-use reasonable way of emulating approx in *Haskell*. The codes are all in *pure* Haskell. The idea is to have a natural feel in writing mathematical code, with operators which just works when working with **Double** and **Float** and their composite types like lists, vectors etc. 

The **Approx** module defines 2 operators **=~** and **/~**, which are for checking *nearly equal to* and *not nearly equal to* respectively. 

## Features
Both the operators **=~** and **/~** are put under the class **Approx**. 

At least one of the operators have to be defined and the other gets automatically defined. 

The library already defines the functions for some of the basic / common types. 

For types where **Eq** is defined like **Char, Bool, Int, Day, Text** the approx is simply replaced with **==**. 

For **Float** and **Double**, the following formula is used,

```
if max ( |x|, |y| ) < epsilon_Zero
then True
else 
  if |x - y| / max ( |x|, |y| ) < epsilon_Eq
  then True
  else False
```

The motivation for defining Approx for classes for which Eq is also defined is to allow for composite types where both Eq and Approx would be present. For example, the following code evaluates to True, even though the tuple is of type **```(Int,Double,Text,[Int],[[Char]],[Double])```**.

```
((2,5.35,"happ",[1,2],["ret","we"],[6.78,3.5]) 
    :: (Int,Double,Text,[Int],[[Char]],[Double])) 
    
    =~ (2,5.35,"happ",[1,2],["ret","we"],[6.78,3.5])
  ```

For UTCTime, the approx operator checks for equality to the nearest minute. The following expression evaluates to **True**.

```
(parseTimeM True defaultTimeLocale "%Y-%m-%d %H:%M:%S" "2020-01-15 15:02:15" 
    :: Maybe UTCTime)

    =~ parseTimeM True defaultTimeLocale "%Y-%m-%d %H:%M:%S" "2020-01-15 15:01:50"
```


The library also provides approx for Complex and common structures like list, boxed and unboxed vector, hashmap, tuples and Maybe. For all lists, tuples, hashmaps and vectors, the approximation is checked right down to the elements and the order for lists and vectors are important. 

For lists, only finite lists are supported. Any use of infinite lists would cause a runtime error. 

There are addtional functions **inRange**, **safeInRange** and **inTol**, which checks for values within Ranges either *explictily* defined as in **inRange** and **safeInRange** or through tolerances as in **inTol**.

## Code examples
The following all expressions evaluate as True
```
(1.0e+7 :: Double) =~ 10000000.05

((1.2, 3.4) :: (Double, Double)) =~ (1.20000008, 3.399999999)
((1.2, 3.5) :: (Double, Double)) /~ (1.20000008, 3.399999999)

([1.2, 3.4, 5.6] :: [Double]) /~ [1.2, 3.4, 5.65]

("star"::Text) =~ ("star"::Text)
("star"::Text) /~ ("start"::Text)

Just (10.00000000000007 :: Double) =~ Just 10.0
Just (10 :: Double) /~ Just 1.0

```


## Installation
Add the library in build-depends and ```import Data.Approx```. 

## Author(s)
Kishaloy Neogi

## License
The library is distributed under the MIT License.

Copyright (c) 2021 Kishaloy Neogi

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.

