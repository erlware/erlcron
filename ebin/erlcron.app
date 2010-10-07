%% -*- Erlang -*-
%% This is the application resource file (.app file) for the ecd_ops application.

{application, erlcron,
  [{description,
    "Erlang Implementation of cron"},

   {vsn, "0.2"},

   {modules,
    [erlcron,
     ecrn_app,
     ecrn_sup,
     ecrn_agent,
     ecrn_test,
     ecrn_control,
     ecrn_cron_sup,
     ecrn_reg]},

   {registered,
    [ecrn_agent]},

   {applications,
    [kernel,
     stdlib,
     sasl,
     eunit]},

   {mod,
    {ecrn_app, []}}
  ]}.
