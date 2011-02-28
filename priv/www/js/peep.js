
var peep = function () {
    var json = {};
    var url = "/web/peep:show";
    var MAX = 9;
    var PLACEHOLDER = "/default.png";
    var slots = _.map(_.range(MAX), function (n) {
            return null;
            });


    return {
        'slots': function (v) {
            return (v ? slots = v : slots);
        },
        'max': function () {
            return (MAX);
        },
        'url': function (v) {
            return (v ? url = v : url);
        },
        'json': function (v) {
            return (v ? json = v : json);
        },
        'placeholder': function () {
            return (PLACEHOLDER);
        },
    };
}();


// { images: [
//     { date: Date, src: IP:Port, dst: IP:Port, uri: Uri },
//     ...])
function get_image_list() {
    $.getJSON(peep.url(), function (json) {
        if (peep.json() != json) {
            redraw(json);
            peep.json(json);
        }
    });
}

function redraw(json) {
    var images = new Array();
    var exists = new Array();
    var nexists = new Array();

    var slots = peep.slots();
    var max = peep.max();

    var nslots = new Array(max);

    images = _.pluck(_.head(json.images, max), 'uri');
    exists = _.intersect(slots, images);

    if (exists.length == max)
        return;

    nexists = _.filter(images, function (n) {
            return _.indexOf(slots, n) == -1;
            });

    nslots = _.map(slots, function (n) {
            return _.indexOf(exists, n) == -1 ? nexists.pop() : n;
            });

    $.each(nslots, function (n) {
        var img = nslots[n] ? nslots[n] : peep.placeholder();
        set_image(img, n);
    });
    peep.slots(nslots);
}

function set_image(image, slot) {
    var id = '#r' + slot;
    var scavenged = '<a href="' + image + '">' +
        '<img src="' + image + '" class="Image" />' +
        '</a>';
    $(id).delay(1000).empty().append(scavenged);
}


