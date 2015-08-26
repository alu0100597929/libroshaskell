<?php

/*
* Register Raleway Google font for fulgent
*/
function fulgent_font_url() {
   $fulgent_font_url = '';
   if ('off' !== _x('on', 'Raleway font: on or off', 'fulgent')) {
       $fulgent_font_url = add_query_arg('family', urlencode('Raleway:400,700,600,800'), "//fonts.googleapis.com/css");
   }
   return $fulgent_font_url;
}
/*
 * fulgent Main Sidebar
*/
function fulgent_widgets_init() {

	register_sidebar( array(
		'name'          => __( 'Main Sidebar', 'fulgent' ),
		'id'            => 'sidebar-1',
		'description'   => __( 'Main sidebar that appears on the right.', 'fulgent' ),
		'before_widget' => '<div class="widget_categories sidebar-widget %2$s" id="%1$s" >',
		'after_widget'  => '</div>',
		'before_title'  => '<h3 class="widget-title">',
		'after_title'   => '</h3>',
	) );
	
	register_sidebar(array(
        'name' => __('Footer Area One', 'fulgent'),
        'id' => 'footer-1',
        'description' => __('Footer Area One that appears on footer.', 'fulgent'),
        'before_widget' => '<aside id="%1$s" class="footer-widget widget %2$s">',
        'after_widget' => '</aside>',
        'before_title' => '<h3 class="widget-title">',
        'after_title' => '</h3>',
    ));
    
    register_sidebar(array(
        'name' => __('Footer Area Two', 'fulgent'),
        'id' => 'footer-2',
        'description' => __('Footer Area Two that appears on footer.', 'fulgent'),
        'before_widget' => '<aside id="%1$s" class="footer-widget widget %2$s">',
        'after_widget' => '</aside>',
        'before_title' => '<h3 class="widget-title">',
        'after_title' => '</h3>',
    ));
    
    register_sidebar(array(
        'name' => __('Footer Area Three', 'fulgent'),
        'id' => 'footer-3',
        'description' => __('Footer Area Three that appears on footer.', 'fulgent'),
        'before_widget' => '<aside id="%1$s" class="footer-widget widget %2$s">',
        'after_widget' => '</aside>',
        'before_title' => '<h3 class="widget-title">',
        'after_title' => '</h3>',
    ));
    
    register_sidebar(array(
        'name' => __('Footer Area Four', 'fulgent'),
        'id' => 'footer-4',
        'description' => __('Footer Area Four that appears on footer.', 'fulgent'),
        'before_widget' => '<aside id="%1$s" class="footer-widget widget %2$s">',
        'after_widget' => '</aside>',
        'before_title' => '<h3 class="widget-title">',
        'after_title' => '</h3>',
    ));

}
add_action( 'widgets_init', 'fulgent_widgets_init' );

/*
 * fulgent Set up post entry meta.
 *
 * Meta information for current post: categories, tags, permalink, author, and date.
 */
function fulgent_entry_meta() {

	$fulgent_category_list = get_the_category_list( ', ',' ');
	
	$fulgent_tag_list = get_the_tag_list('<li>'.__('Tags : ','fulgent'),', ',' '.'</li>');
	$fulgent_date = sprintf( '<time datetime="%1$s">%2$s</time>',
		esc_attr( get_the_date( 'c' ) ),
		esc_html( get_the_date() )
	);

	$fulgent_author = sprintf( '<a href="%1$s" title="%2$s" >%3$s</a>',
		esc_url( get_author_posts_url( get_the_author_meta( 'ID' ) ) ),
		esc_attr( sprintf( __( 'View all posts by %s', 'fulgent' ), get_the_author() ) ),
		get_the_author()
	);
	
	if ($fulgent_tag_list) {
		$fulgent_utility_text = '<div class="post-meta"><ul>
			<li> ' . __('by','fulgent').' : %4$s </li>	
			<li>'. __('Posted in','fulgent') .' : %1$s </li>
			%2$s 
			<li> '.fulgent_comment_number_custom().'</li>
			</ul>
		</div>';
	} elseif ( $fulgent_category_list ) {
		$fulgent_utility_text = '<div class="post-meta"><ul>
			<li>'.__('by','fulgent').' : %4$s</li>
			<li>'.__('Posted in','fulgent').' : %1$s </li>
			 %2$s  
			<li>'.fulgent_comment_number_custom().'</li>
			</ul>
		</div>';
	} else {
		$fulgent_utility_text = '<div class="post-meta"><ul>
			<li>'.__('by','fulgent').' : %4$s </li>
			<li>'. __('Posted on','fulgent').' : %3$s </li>
			 %2$s 
			<li>'.fulgent_comment_number_custom().'</li>
			</ul>
		</div>';
	}
	printf(
		$fulgent_utility_text,
		$fulgent_category_list,
		$fulgent_tag_list,
		$fulgent_date,
		$fulgent_author
	);
}
function fulgent_comment_number_custom(){
$fulgent_num_comments = get_comments_number(); // get_comments_number returns only a numeric value
$fulgent_comments=__('No Comments','fulgent');
if ( comments_open() ) {
	if ( $fulgent_num_comments == 0 ) {
		$fulgent_comments = __('No Comments','fulgent');
	} elseif ( $fulgent_num_comments > 1 ) {
		$fulgent_comments = $fulgent_num_comments . __(' Comments','fulgent');
	} else {
		$fulgent_comments = __('1 Comment','fulgent');
	}
}
return $fulgent_comments;
}
/*
 * Comments placeholder function
 * 
**/
add_filter( 'comment_form_default_fields', 'fulgent_comment_placeholders' );

function fulgent_comment_placeholders( $fields )
{
	$fields['author'] = str_replace(
		'<input',
		'<input placeholder="'
		/* Replace 'theme_text_domain' with your theme’s text domain.
		* I use _x() here to make your translators life easier. :)
		* See http://codex.wordpress.org/Function_Reference/_x
		*/
		. _x(
		'Name *',
		'comment form placeholder',
		'fulgent'
		)
		. '" required',
		
	$fields['author']
	);
	$fields['email'] = str_replace(
		'<input',
		'<input placeholder="'
		. _x(
		'Email Id *',
		'comment form placeholder',
		'fulgent'
		)
		. '" required',
	$fields['email']
	);
	$fields['url'] = str_replace(
		'<input',
		'<input placeholder="'
		. _x(
		'Website URl',
		'comment form placeholder',
		'fulgent'
		)
		. '" required',
	$fields['url']
	);
	
	return $fields;
}
add_filter( 'comment_form_defaults', 'fulgent_textarea_insert' );
	function fulgent_textarea_insert( $fields )
	{
		$fields['comment_field'] = str_replace(
			'<textarea',
			'<textarea  placeholder="'
			. _x(
			'Comment',
			'comment form placeholder',
			'fulgent'
			)
		. '" ',
		$fields['comment_field']
		);
	return $fields;
	}
function fulgent_pagination()
{
	the_posts_pagination( array(
				'prev_text'          => __( 'Previous', 'fulgent' ),
				'next_text'          => __( 'Next', 'fulgent' ),
				'before_page_number' => '<span class="meta-nav screen-reader-text">' . ' ' . ' </span>',
			) );  
}
?>
