<?php 
/*
 * Impressive Enqueue css and js files
*/
function fulgent_enqueue()
{
	wp_enqueue_style('fulgent-bootstrap',get_template_directory_uri().'/css/bootstrap.css',array());
	wp_enqueue_style('fulgent-font-awesome',get_template_directory_uri().'/css/font-awesome.css',array());
	wp_enqueue_style('fulgent-theme',get_template_directory_uri().'/css/theme.css',array());
	wp_enqueue_style('fulgent-style',get_stylesheet_uri(),array());
	
	wp_enqueue_script('fulgent-default',get_template_directory_uri().'/js/default.js',array('jquery'));
	wp_enqueue_script('fulgent-bootstrapjs',get_template_directory_uri().'/js/bootstrap.js',array('jquery'));
	
	if(is_page_template('page-templates/frontpage.php')){    
		wp_enqueue_script('fulgent-owlcarousel',get_template_directory_uri().'/js/owl.carousel.js',array('jquery'));
	}
	if ( is_singular() ) wp_enqueue_script( "comment-reply" ); 
}
add_action('wp_enqueue_scripts', 'fulgent_enqueue');
