function sendRows(action, gridId, name) {
	var grid = $("#" + gridId);
	var selData = grid.datagrid("getSelections");
	var opt = grid.datagrid("options");

	var idField = opt["returnField"];
	if (!idField) {
		idField = opt["idField"];
	}
	if (idField) {
		var titleField = opt["titleField"];
		if (!titleField) {
			titleField = idField;
		}
		var kvList = [];
		try {
			for (var r = 0; r < selData.length; r++) {
				if (!selData[r][idField]) {
					throw "Null value for '" + idField + "' column as return key in your '" + (r + 1) + "' selected row!";
				}
				var obj = {};
				obj["key"] = selData[r][idField];
				obj["value"] = selData[r][titleField];
				obj["row"] = selData[r];
				kvList.push(obj);
			}

			if (typeof window[name + "SelValidJSAll"] === "function") {
				window[name + "SelValidJSAll"](kvList);
			}

			var result = {};
			result["action"] = action;
			result["data"] = kvList;
			parent.postMessage(JSON.stringify(result), '*');
		} catch (e) {
			$.messager.alert("Error", e);
		}
	} else {
		$.messager.alert("Error", "No idField for grid!");
	}
}