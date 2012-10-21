{application,erlcron,
             [{description,"Erlang Implementation of cron"},
              {vsn,"0.2"},
              {modules,[ecrn_agent,ecrn_app,ecrn_control,ecrn_cron_sup,
                        ecrn_reg,ecrn_sup,ecrn_test,ecrn_util,erlcron]},
              {registered,[ecrn_agent]},
              {applications,[kernel,stdlib,sasl,eunit]},
              {mod,{ecrn_app,[]}}]}.
