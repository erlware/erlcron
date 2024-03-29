%% compatibility
-ifdef(OTP_RELEASE). %% this implies 21 or higher
-define(EXCEPTION(Class, Reason, Stacktrace), Class:Reason:Stacktrace).
-define(GET_STACK(Stacktrace), Stacktrace).
-include_lib("kernel/include/logger.hrl").
-else.
-define(EXCEPTION(Class, Reason, _), Class:Reason).
-define(GET_STACK(_),        erlang:get_stacktrace()).
-define(LOG_ERROR(Report),   error_logger:error_report(Report)).
-define(LOG_WARNING(Report), error_logger:warning_report(Report)).
-define(LOG_INFO(Report),    error_logger:info_report(Report)).
-endif.
