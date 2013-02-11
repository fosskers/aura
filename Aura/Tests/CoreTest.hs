import Aura.Settings.SettingsTest
import Aura.Monad.Aura
import Aura.Core

import Control.Monad

---

test_getDevelPkgs = runAura getDevelPkgs sampleSettings

-- Should produce `Right []`
test01 = runAura ((map fst `liftM` getForeignPackages) >>= filterRepoPkgs)
         sampleSettings

test02 = runAura (namespaceOf `liftM` aurPkg "dwarffortress-ironhand") sampleSettings