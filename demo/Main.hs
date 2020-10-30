{-# LANGUAGE
  QuasiQuotes, TemplateHaskell,
  FlexibleInstances, MultiParamTypeClasses,
  DataKinds, TypeFamilies,
  UndecidableInstances
  #-}
module Main
where

import Prelude
import Domain
import DomainOptics
import Optics


main =
  return ()

declare Nothing labelOpticDeriver
  [schema|

    ServiceAddress:
      sum:
        network: NetworkAddress
        local: DomainSocketPath

    NetworkAddress:
      product:
        protocol: TransportProtocol
        host: Host
        port: Word16

    DomainSocketPath:
      product:
        path: FilePath

    TransportProtocol:
      enum:
        - tcp
        - udp

    Host:
      sum:
        ip: Ip
        name: Text

    Ip:
      sum:
        v4: Word32
        v6: Word128

    Word128:
      product:
        part1: Word64
        part2: Word64

    |]
