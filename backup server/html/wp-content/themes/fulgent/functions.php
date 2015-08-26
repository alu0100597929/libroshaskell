<?php 
/*
 * Set up the content width value based on the theme's design.
 */
if ( ! function_exists( 'fulgent_setup' ) ) :
function fulgent_setup() {
	
	// This theme uses wp_nav_menu() in one locations.
	register_nav_menus( array(
		'primary'   => __( 'Main Menu', 'fulgent' ),	
	) );
	global $content_width;
	if ( ! isset( $content_width ) ) $content_width = 1170;

	/*
	* Make fulgent theme available for translation.
	*/
	load_theme_textdomain( 'fulgent', get_template_directory() . '/languages' );
	
	// This theme styles the visual editor to resemble the theme style.
	add_editor_style(array('css/editor-style.css', fulgent_font_url()));
	
	// Add RSS feed links to <head> for posts and comments.
	add_theme_support('automatic-feed-links');
	add_theme_support( 'title-tag' );
	add_theme_support('post-thumbnails');
	set_post_thumbnail_size(672, 372, true);
	add_image_size('fulgent-full-width', 1110, 576, true);
	add_image_size('fulgent-home-blog-image', 550, 275, true);
	add_image_size('fulgent-latest-posts-widget', 50, 50, true);
	/*        
	* Switch default core markup for search form, comment form, and comments        
	* to output valid HTML5.        
	*/
	add_theme_support('html5', array(
	   'search-form', 'comment-form', 'comment-list',
	));
	// Add support for featured content.
	add_theme_support('featured-content', array(
	   'featured_content_filter' => 'fulgent_get_featured_posts',
	   'max_posts' => 6,
	));
	
	
	add_theme_support( 'custom-background', apply_filters( 'fulgent_custom_background_args', array(
	'default-color' => 'f8f8f8',
	) ) );
	// This theme uses its own gallery styles.       
	add_filter('use_default_gallery_style', '__return_false');   	
}

endif; // fulgent_setup
add_action( 'after_setup_theme', 'fulgent_setup' );

			
/***  excerpt Length ***/ 
function fulgent_change_excerpt_more( $more ) {
    return '<div class="theme-button"><a class="transparent-btn" href="'. get_permalink() . '" >'.__('READ MORE','fulgent').'</a></div>';
}
add_filter('excerpt_more', 'fulgent_change_excerpt_more');


add_action('wp_head','fulgent_purpose_bg_img_css');
function fulgent_purpose_bg_img_css()
{
	$fulgent_breadcrumbs_image_bg=get_theme_mod('fulgent_breadcrumbs_image_bg');
	if (!empty($fulgent_breadcrumbs_image_bg) ){
		$fulgent_breadcrumbs_image_bg = esc_url(get_theme_mod('fulgent_breadcrumbs_image_bg'));
		$fulgent_breadcrumbs_output="<style> .moto-section { background-image :url('".$fulgent_breadcrumbs_image_bg."');
		background-position: center;} </style>";
		echo $fulgent_breadcrumbs_output;
	}
	
	$fulgent_homebanner_image_bg=get_theme_mod('fulgent_homebanner_image_bg');
	if (!empty($fulgent_homebanner_image_bg) ){
		$fulgent_homebanner_image_bg = esc_url(get_theme_mod('fulgent_homebanner_image_bg'));
		$fulgent_homebanner_breadcrumbs_output="<style> .master-head { background-image :url('".$fulgent_homebanner_image_bg."');
		background-position: center;} </style>";
		echo $fulgent_homebanner_breadcrumbs_output;
	}
}


/*** Enqueue css and js files ***/
require get_template_directory() . '/functions/enqueue-files.php';

/*** Theme Default Setup ***/
require get_template_directory() . '/functions/theme-default-setup.php';

/*** Breadcrumbs ***/
require get_template_directory() . '/functions/breadcrumbs.php';

/* CUSTOM POST WIDGET FOR LATEST POST */
require get_template_directory() . '/functions/latestpost.php';

/* get in touch start */
require get_template_directory() . '/functions/getintouch.php';

/*** Custom Header ***/
require get_template_directory() . '/functions/custom-header.php';

/*** Customizer ***/
require get_template_directory() . '/functions/theme-customizer.php';

/*** Customizer ***/
require get_template_directory() . '/theme-options/theme-option.php';
	