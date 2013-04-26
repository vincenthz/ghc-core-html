jQuery(document).ready(function() {
    jQuery(".idxdir > ul").hide();
    jQuery(".idxdir > span").click(function() {
	jQuery(this).next("ul").slideToggle(200);
    });
    jQuery("#buttonToggleBody").click(function() {
	jQuery("pre.body").slideToggle(200);
    });
    jQuery(".binding > .header").click(function() {
	jQuery(this).next(".body").slideToggle(200);
    });
});
