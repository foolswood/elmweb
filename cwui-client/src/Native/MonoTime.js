//import Native.Scheduler //

var _foolswood$elmweb$Native_MonoTime = function() {

var rightNow = function() {
    return performance.now() / 1000;
}

var now = _elm_lang$core$Native_Scheduler.nativeBinding(function(callback)
{
	callback(_elm_lang$core$Native_Scheduler.succeed(rightNow));
});

return {now : now, rightNow : rightNow};
}();
