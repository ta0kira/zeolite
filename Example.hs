import Resolver
import Unresolved
import Variance

uIterator = UnresolvedTypeClass {
    utcName = "Iterator",
    utcParams = [
        UnresolvedTypeParam { utpName = "x", utpVariance = Covariant }
      ],
    utcInherits = [
        UnresolvedType {
          utTypeClass = "Reader",
          utParamArgs = [UnresolvedTypeArg { utName = "x" }]
        }
      ],
    utcFilters = [
      UnresolvedParamFilter {
        upfName = "x",
        upfType = UnresolvedType {
            utTypeClass = "Iterator",
            utParamArgs = [UnresolvedTypeArg { utName = "x" }]
          }
      }
    ]
  }

uWriter = UnresolvedTypeClass {
    utcName = "Writer",
    utcParams = [
        UnresolvedTypeParam { utpName = "x", utpVariance = Contravariant }
      ],
    utcInherits = [],
    utcFilters = []
  }

uReader = UnresolvedTypeClass {
    utcName = "Reader",
    utcParams = [
        UnresolvedTypeParam { utpName = "x", utpVariance = Covariant }
      ],
    utcInherits = [],
    utcFilters = []
  }

uQueue = UnresolvedTypeClass {
    utcName = "Queue",
    utcParams = [
        UnresolvedTypeParam { utpName = "x", utpVariance = Invariant }
      ],
    utcInherits = [
        UnresolvedType {
          utTypeClass = "Writer",
          utParamArgs = [UnresolvedTypeArg { utName = "x" }]
        },
        UnresolvedType {
          utTypeClass = "Reader",
          utParamArgs = [UnresolvedTypeArg { utName = "x" }]
        }
      ],
    utcFilters = [
      UnresolvedParamFilter {
        upfName = "x",
        upfType = UnresolvedType {
            utTypeClass = "Writer",
            utParamArgs = [UnresolvedTypeArg { utName = "x" }]
          }
      }
    ]
  }

uFunction = UnresolvedTypeClass {
    utcName = "Function",
    utcParams = [
        UnresolvedTypeParam { utpName = "x", utpVariance = Contravariant },
        UnresolvedTypeParam { utpName = "y", utpVariance = Covariant }
      ],
    utcInherits = [],
    utcFilters = []
  }

testType = resolve $ UnresolvedType {
    utTypeClass = "Writer",
    utParamArgs = [
      UnresolvedType {
        utTypeClass = "Queue",
        utParamArgs = [
          UnresolvedType {
            utTypeClass = "Function",
            utParamArgs = [
              UnresolvedTypeArg { utName = "x" },
              UnresolvedTypeArg { utName = "y" }
            ]
          }
        ]
      }
    ]
  }

testType2 = resolve $ UnresolvedType {
    utTypeClass = "Writer",
    utParamArgs = [UnresolvedTypeArg { utName = "x" }]
  }

testType3 = resolve $ UnresolvedTypeArg { utName = "x" }

resolve x = graph >>= \g -> return $ resolveTypeClassInstance g x
graph = createTypeClassGraph [uWriter, uReader, uQueue, uFunction, uIterator]
