//import Native.Scheduler //

var _foolswood$elmweb$Native_MonoTime = function() {

var now = _elm_lang$core$Native_Scheduler.nativeBinding(function(callback)
{
	callback(_elm_lang$core$Native_Scheduler.succeed(performance.now()));
});

return {now : now};
}();
