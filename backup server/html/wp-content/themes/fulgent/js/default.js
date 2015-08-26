/****/
function headeranim() {
    if (jQuery(window).width() > 767 && (jQuery(document).scrollTop() > jQuery("header").height())) {
        jQuery("header").css({position: "fixed"});
        jQuery("header").next().css({"padding-top":jQuery("header").height()});
        jQuery("header").slideDown().css({'background-color':'rgba(0,0,0,0.8)'});
        
    }
    else {
        jQuery("header").css({"position": 'relative'});
        jQuery("header").next().css({"padding-top": '0'});
        jQuery("header").css({'background-color':'transparent'});
        jQuery(".pre-header").css({'background-color':'#000000'});
        
    }
}
jQuery(document).ready(function () {
    jQuery(".go-top").click(function () {
        jQuery("html,body").animate({scrollTop: 0}, 1e3);
    });
     jQuery(".arrow-up-click,.click-down").click(function () {
        jQuery("html,body").animate({scrollTop: jQuery(this).parents().parents().parents().next().offset().top - 105}, 1e3);
    });
        
        
    headeranim();
});
jQuery(window).scroll(function (){
     headeranim();
});
jQuery(window).resize(function () {
    headeranim();
});

jQuery(document).ready(function () {
    var $window = jQuery(window);
    var windowHeight = $window.height();

    $window.resize(function () {
        windowHeight = $window.height();
    });

    jQuery('.parallax-fulgent[data-type="background"]').each(function () {
        var windowHeight = $window.height();
        $window.resize(function () {
            windowHeight = $window.height();
        });
        var $bgobj = jQuery(this); // assigning the object
        var $element = jQuery(this);
        var top = $element.offset().top;
        var height = $element.height();
        var firstTop;
        var speedFactor = $bgobj.data('speed');

        if (arguments.length < 2 || speedFactor === null)
            speedFactor = 1;
        $element.each(function () {
            firstTop = $element.offset().top;
        });

        jQuery(window).scroll(function () {
            var pos = $window.scrollTop();

            // Check if totally above or totally below viewport
            if (top + height < pos || top > pos + windowHeight) {
                return;
            }
            var yPos = Math.round((firstTop - pos) / speedFactor);
            // Put together our final background position
            var coords = '50% ' + yPos + 'px';
            // Move the background
            $bgobj.css({backgroundPosition: coords});
        });
    });
});

