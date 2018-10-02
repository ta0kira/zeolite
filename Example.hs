import Types
import Variance

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
    utcFilters = []
  }

resolvedTypeClasses = newTypeResolver [uWriter, uReader, uQueue]
