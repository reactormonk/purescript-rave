# Rave

The base type of this library is `Rave`, which is short for `Reader`, `Aff`,
`Variant`. Combined, `Rave` is a standard stack for PureScript application
development. It implements
[ReaderT](https://www.fpcomplete.com/blog/2017/06/readert-design-pattern)
and
[Checked Exceptions](https://pursuit.purescript.org/natefaubion/purescript-checked-exceptions).
Rave does not support
[interpeters](https://pursuit.purescript.org/packages/purescript-run),
but it would be possible to have `purescript-run` interpret into `Rave`.

```purescript
newtype Rave r v e = Rave (ReaderT r (ExceptV v Aff) e)
```

## ReaderT

The ReaderT part allows you to pass configuration along to your application
without having to do so manually, which tends to clutter your codebase.

```purescript
type Config =
  { port :: Int
  , host :: String
  , tokenInfoUrl :: String
  , elasticSearchUrl :: String
  }

config :: Config
config =
  { port: 8080
  , host: "localhost"
  , tokenInfoUrl: "http://localhost:8081/tokeninfo"
  , elasticSearchUrl: "http://localhost:9200"
  }
```

With `Rave` and partial records, this code can be rewritten as

```purescript
config =
  { port: 8080
  , host: "localhost"
  , databaseString: "postgresql://postgres@localhost"
  }

main = runRave (RProxy :: _ ()) config do
  dbConnection <- openConnection
  startServer $ { dbConnection: DbConnection, host: config.host, port: config.port }

openConnection :: forall r. Rave { databaseString :: String | r } () DbConnection
startServer :: forall r. Rave { port :: Int, host :: String, dbConnection :: String | r } () -> Aff ()
```

## Variant (checked exceptions)

Java has given checked exceptions a bad reputation. In PureScript, they have a
comparatively low cognitive and LoC overhead.

## Aff (effect)

Aff is a good default effect monad for any application. You may hit a few corner
cases with JavaScript APIs having timing issues with privileges (e.g.
`window.open`), but 99% of the cases, it should be fine.
