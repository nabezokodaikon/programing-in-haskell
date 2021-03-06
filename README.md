# programing-in-haskell
プログラミングHaskellを読んで学習。
* [Programming in Haskell](http://www.cs.nott.ac.uk/~pszgmh/pih.html)

## 参考
### ビルドツール
* [The Haskell Tool Stack](https://docs.haskellstack.org/en/stable/README/)
* [Haskell Stack](https://haskell.e-bigmoon.com/stack/)
### テスト系
* [HSpec: A Testig Framework for Haskell](https://hspec.github.io/)
* [hspec-example](https://github.com/hspec/hspec-example)
* [Hspec](https://hspec.github.io/)
### 記事
* [本気で Haskell したい人向けの Stack チュートリアル](https://qiita.com/waddlaw/items/49874f4cf9b680e4b015)
* [「プログラミングHaskell」を読んだ](https://orangain.hatenablog.com/entry/programming-in-haskell)

## プロジェクト作成
```
$ stack new my-project
$ cd my-project
$ stack setup
$ stack build
```

## Tag
```
$ stack install hasktags
$ ~/.local/bin/hasktags . -c
```

## Test
### HSpec
#### 1. ファイルの関連付け
srcファイルとSpecファイルを関連付けるために、 test/Spec.hsファイルに以下を記述。(他の行は削除する。)
```
{-# OPTIONS_GHC -F -pgmF hspec-discover #-}
```
#### 2. テスト準備
1. `package.yaml`に`hspec`を追記。 
```
tests:
  programing-in-haskell-test:
    main:                Spec.hs
    source-dirs:         test
    ghc-options:
    - -threaded
    - -rtsopts
    - -with-rtsopts=-N
    dependencies:
    - programing-in-haskell
    - hspec # <-
```
2. テスト初回実行(テスト一括実行)
```
# stack test
```
3. 個別テスト
```
$ stack runghc test/Example/ExampleSpec.hs
```
