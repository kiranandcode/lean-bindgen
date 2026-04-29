import Clingo.Types

namespace Clingo

inductive ExternalType where | AssignFreely | AssignTrue | AssignFalse | Release

namespace Backend

-- Backend operations are stubs (matching cleango).
-- These would require additional hand-written shim code.

end Backend

end Clingo
