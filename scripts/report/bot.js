var Twit = require('twit');

var config = require('../../config');

var T = new Twit(config);

var rct = fs.readFileSync('../../rig_count_timeline.png', { encoding: 'base64' });
var rcm = fs.readFileSync('../../rig_count_map.png', { encoding: 'base64' });

T.post('media/upload', { media_data: [rct, rcm] }, function (err, data, response) {
 var mediaIdStr = data.media_id_string;
 var altText = "Rig count.";
 var meta_params = { media_id: mediaIdStr, alt_text: { text: altText } };
 
 T.post('media/metadata/create', meta_params, function (err, data, response) {
    if (!err) {
     
     var params = { status: "This weeks rig counts #OOTT #rigcount #oilandgas",
        media_ids: mediaIDStr
      };
     
     T.post('statuses/update', params, function (err, data, response) { console.log(data); });
     
    }
 });
 
});
