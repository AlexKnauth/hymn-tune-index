// needs `tree` defined as the JSON object from
// `hymn-tune-index.json`

function tree_get(indexes) {
    var hymns = [];
    var node = tree;
    for (var i of indexes) {
        let next = node[i];
        if (next) {
            push_all(hymns, node[""]);
            node = next;
        } else {
            return push_node_hymns(hymns, node);
        }
    }
    return push_node_hymns(hymns, node);
}

function push_node_hymns(hymns, node) {
    if (!node) { return hymns; }
    push_all(hymns, node[""]);
    for (var i = 0; i < 12; i += 1) {
        push_node_hymns(hymns, node[i]);
    }
    return hymns;
}

function push_all(arr, elems) {
    if (!elems) { return arr; }
    for (var elem of elems) {
        arr.push(elem);
    }
    return arr;
}

function mod(x, n) {
    let r = x % n;
    return r < 0 ? r + n : r;
}

function parse_scale_degree(str) {
    if (str.length === 0) {
        return 0;
    } else if (str[0] === '#') {
        return parse_scale_degree(str.slice(1)) + 1;
    } else if (str[0] === 'b') {
        return parse_scale_degree(str.slice(1)) - 1;
    } else {
        let i = parseInt(str);
        if (!Number.isInteger(i)) { return 0; }
        let major = [0,2,4,5,7,9,11];
        return major[mod(i - 1, 7)];
    }
}

function parse_scale_degrees(str) {
    let trimmed = str.trim();
    if (trimmed.length === 0) { return []; }
    let strs = trimmed.split(/\s+/);
    return strs.map(parse_scale_degree);
}

function onkey_hymntune() {
    if (event.key === 'Enter' || event.which === 13 || event.keyCode === 13) {
        onclick_hymntune();
    }
}

function onclick_hymntune() {
    let input = document.getElementById("hymntuneinput");
    let scale_degs = parse_scale_degrees(input.value);
    let hymns = tree_get(scale_degs);
    let hymntunetable = document.getElementById("hymntunetable");
    let header_row = document.createElement("TR");
    header_row.innerHTML = "<th>Book</th> <th>Num</th> <th>Tune</th>";
    var rows = [header_row];
    for (var hymn of hymns) {
        let hymnal = hymn[0];
        let number = hymn[1];
        let tune = hymn[2];
        let row = document.createElement("TR");
        let hymnalElement = document.createElement("TD");
        hymnalElement.textContent = hymnal.toString();
        let numberElement = document.createElement("TD");
        numberElement.textContent = number.toString();
        let tuneElement = document.createElement("TD");
        tuneElement.textContent = tune.join(" ");
        row.appendChild(hymnalElement);
        row.appendChild(numberElement);
        row.appendChild(tuneElement);
        rows.push(row);
    }
    while (hymntunetable.firstChild) {
        hymntunetable.removeChild(hymntunetable.firstChild);
    }
    for (var row of rows) {
        hymntunetable.appendChild(row);
    }
}
