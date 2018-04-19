var _IORefs = {counter : 0, items : {}};

var _foolswood$elmweb$Native_DragAndDrop = function () {

var stopPropagation = function (evt) {
    evt.stopPropagation();
}

var preventDefault = function (evt) {
    evt.preventDefault();
}

var getEffectAllowed = function (evt) {
    return evt.effectAllowed;
}

var setEffectAllowed = function (evt) {
    return function (val) {
        evt.dataTransfer.effectAllowed = val;
    }
}

var getDataTransfer = function (evt) {
    var r = {};
    var ts = evt.dataTransfer.types;
    for (var i = 0; i < ts.length; i++) {
        r[ts[i]] = evt.dataTransfer.getData(ts[i]);
    }
    return JSON.stringify(r);
}

var setDataTransfer = function (evt) {
    return function (dataTransferString) {
        var dataTransfer = JSON.parse(dataTransferString);
        for (var mimeType in dataTransfer) {
            evt.dataTransfer.setData(mimeType, dataTransfer[mimeType]);
        }
    }
}

var newRef = function (elmVal) {
    var ref = (_IORefs.counter++).toString();
    _IORefs.items[ref] = elmVal;
    return ref;
}

return {stopPropagation : stopPropagation, preventDefault : preventDefault, getEffectAllowed : getEffectAllowed, setEffectAllowed : setEffectAllowed, getDataTransfer : getDataTransfer, setDataTransfer : setDataTransfer, newRef : newRef};
}();

var callRef = function (ref, a) {
    return _IORefs.items[ref](a);
}

this["Foolswood"] = {callRef : callRef};
