package checkstyle.utils;

class StringUtils {

	public static inline function splitAll(s:String, delimiters:Array<String>, includeDelimeters = false):Array<String> {
		var result = [];

		var lastSplit = 0;
		for (i in 0...s.length) {
			for (d in delimiters) {
				if (s.substr(i, d.length) == d) { //found delimiter
					result.push(s.substring(lastSplit, i));
					if (includeDelimeters) result.push(s.substr(i, d.length));
					lastSplit = i + d.length;
				}
			}
		}
		result.push(s.substring(lastSplit));

		return result;
	}

	public static inline function contains(s:String, c:String):Bool {
		return s.indexOf(c) != -1;
	}

	public static function isStringInterpolation(s:String, fileContent:String, pos:Position):Bool {
		var quote:String = fileContent.substr(pos.min, 1);
		if (quote != "'") return false;
		var regex:EReg = ~/(^|[^$])\$(\{|[a-zA-Z0-9_]+)/;
		return regex.match(s);
	}
}