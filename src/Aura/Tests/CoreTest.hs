import Aura.Settings.SettingsTest
import Aura.Monad.Aura
import Aura.Core

---

test_getDevelPkgs = runAura getDevelPkgs sampleSettings

-- Should produce `Right []`
test01 = runAura ((map fst `fmap` getForeignPackages) >>= filterRepoPkgs)
         sampleSettings

test02 = runAura (namespaceOf `fmap` aurPkg "dwarffortress-ironhand") sampleSettings