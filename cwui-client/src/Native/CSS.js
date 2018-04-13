var _foolswood$elmweb$Native_CSS = function() {

var cachedEm;
var emPx = function() {
    if (cachedEm === undefined) {
        cachedEm = parseFloat(getComputedStyle(document.body).fontSize);
    }
    return cachedEm;
}

var styleEl = document.createElement('style');
document.head.appendChild(styleEl);
var styleSheet = styleEl.sheet;

var insertRule = function(obj) {
    styleSheet.insertRule(obj.rule, obj.idx);
}

var deleteRule = function(idx) {
    styleSheet.deleteRule(idx);
}

var mVars = {};

var swapMvar = function(obj) {
    var rv = mVars[obj.key];
    mVars[obj.key] = obj.value;
    if (rv === undefined) {
        return {hasValue: false};
    } else {
        return {hasValue: true, value: rv};
    }
}

return {emPx: emPx, insertRule: insertRule, deleteRule: deleteRule, swapMvar: swapMvar};
}();
