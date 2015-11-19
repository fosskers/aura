import Aura.Settings.SettingsTest
import Aura.Monad.Aura
import Aura.Core

---

test_getDevelPkgs = runAura getDevelPkgs sampleSettings

-- Should produce `Right []`
test01 = runAura ((fmap fst <$> getForeignPackages) >>= filterRepoPkgs)
         sampleSettings

test02 = runAura (namespaceOf <$> aurPkg "dwarffortress-ironhand") sampleSettings
