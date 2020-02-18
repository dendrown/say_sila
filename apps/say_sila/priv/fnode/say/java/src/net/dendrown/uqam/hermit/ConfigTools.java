
package net.dendrown.uqam.hermit;

import org.semanticweb.HermiT.Configuration;
import org.semanticweb.HermiT.Configuration.TableauMonitorType;


public class ConfigTools
{
    static public Configuration reconfig()
    {
        Configuration cfg = new Configuration();

        // FIXME: the tableauMonitorType field seems to be final w/ a value of NONE
        //        even though the source on github doesn't reflect this.
        //cfg.tableauMonitorType=TableauMonitorType.TIMING;

        return cfg;
    }
}
