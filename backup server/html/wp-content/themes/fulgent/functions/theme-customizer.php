<?php
function fulgent_theme_customizer( $wp_customize ) {

    $wp_customize->add_section( 'fulgent_basic_section' , array(
    'title'       => __( 'Basic Settings', 'fulgent' ),
    'priority'    => 30,
	) );
        
	
	$wp_customize->add_panel( 'fulgent_home_id', array(
		'capability'     => 'edit_theme_options',
		'theme_supports' => '',
		'title'          => __('Home Page Settings','fulgent' ),
		'description'    => '',
		'priority'    => 30,
	) );
	
	$wp_customize->add_section( 'fulgent_homebanner_section' , array(
		'title'       => __( 'Home Banner', 'fulgent' ),
		'priority'    => 30,
		'panel'  => 'fulgent_home_id',
	) );
	$wp_customize->add_section( 'fulgent_aboutus_section' , array(
		'title'       => __( 'About Us', 'fulgent' ),
		'priority'    => 30,
		'panel'  => 'fulgent_home_id',
	) );
	$wp_customize->add_section( 'fulgent_mainmoto_section' , array(
		'title'       => __( 'Our Main Moto', 'fulgent' ),
		'priority'    => 30,
		'panel'  => 'fulgent_home_id',
	) );
	$wp_customize->add_section( 'fulgent_blog_section' , array(
		'title'       => __( 'Blog Section', 'fulgent' ),
		'priority'    => 30,
		'panel'  => 'fulgent_home_id',
	) );

	/* basic section */
	/*theme logo*/
	$wp_customize->add_setting( 'fulgent_logo' ,array(
		'sanitize_callback' => 'esc_url_raw',
		)
	 );
    $wp_customize->add_control( new WP_Customize_Image_Control( $wp_customize, 'fulgent_logo', array(
		'label'    => __( 'Logo (Recommended size 220 x 120)', 'fulgent' ),
		'section'  => 'fulgent_basic_section',
		'settings' => 'fulgent_logo',
	) ) );

	// blog title
	$wp_customize->add_setting( 'fulgent_blogtitle', array(
            'default'        => ' ',
            'sanitize_callback' => 'esc_attr',
        ) );
   $wp_customize->add_control( 'fulgent_blogtitle', array(
		'label'   => __('Blog Title','fulgent' ),
		'section' => 'fulgent_basic_section',
		'type'    => 'text',
        ) );

	// copyright
	$wp_customize->add_setting( 'copyright_url_setting', array(
		'default'        => '',
		'sanitize_callback' => 'esc_html',
	) );
	$wp_customize->add_control( 'copyright_url_setting', array(
		'label'   => 'Copyright text',
		'section' => 'fulgent_basic_section',
		'type'    => 'text'
	) );


     // Home Banner
	 $wp_customize->add_setting( 'fulgent_homebanner_image_bg',array(
		'sanitize_callback' => 'esc_url_raw',
		)
	);
    $wp_customize->add_control( new WP_Customize_Image_Control( $wp_customize, 'fulgent_homebanner_image_bg', array(
			'label'    => __( 'Banner  Image (Recommended size 1280 x 853)', 'fulgent' ),
			'section'  => 'fulgent_homebanner_section',
			'settings' => 'fulgent_homebanner_image_bg',
		) 
	) );  
     $wp_customize->add_setting( 'fulgent_banner_title1', array(
		'default'        => '',
		'sanitize_callback' => 'esc_attr',
	) );
    $wp_customize->add_control( 'fulgent_banner_title1', array(
		'label'   => __('Banner Title 1','fulgent' ),
		'section' => 'fulgent_homebanner_section',
		'type'    => 'text',
    ) );
    
     $wp_customize->add_setting( 'fulgent_banner_title2', array(
		'default'        => '',
		'sanitize_callback' => 'esc_attr',
	) );
    $wp_customize->add_control( 'fulgent_banner_title2', array(
		'label'   => __('Banner Title 2','fulgent' ),
		'section' => 'fulgent_homebanner_section',
		'type'    => 'text',
    ) );

     $wp_customize->add_setting( 'fulgent_banner_title3', array(
		'default'        => '',
		'sanitize_callback' => 'esc_attr',
	) );
    $wp_customize->add_control( 'fulgent_banner_title3', array(
		'label'   => __('Banner Title 3','fulgent' ),
		'section' => 'fulgent_homebanner_section',
		'type'    => 'text',
    ) );	
   
     $wp_customize->add_setting( 'fulgent_banner_button', array(
		'default'        => '',
		'sanitize_callback' => 'esc_attr',
	) );
    $wp_customize->add_control( 'fulgent_banner_button', array(
		'label'   => __('Banner Button','fulgent' ),
		'section' => 'fulgent_homebanner_section',
		'type'    => 'text',
    ) );
	
	$wp_customize->add_setting( 'fulgent_banner_link', array(
		'default'        => '',
		'sanitize_callback' => 'esc_url',
	) );
    $wp_customize->add_control( 'fulgent_banner_link', array(
		'label'   => __('Banner Button Link','fulgent' ),
		'section' => 'fulgent_homebanner_section',
		'type'    => 'text',
    ) );
	 
	

     //Our path to mainmoto
	$wp_customize->add_setting( 'fulgent_mainmototitle', array(
		'default'        => '',
		'sanitize_callback' => 'esc_attr',
	) );
    $wp_customize->add_control( 'fulgent_mainmototitle', array(
		'label'   => __('Title','fulgent' ),
		'section' => 'fulgent_mainmoto_section',
		'type'    => 'text',
    ) );
	
	 $wp_customize->add_setting( 'fulgent_mainmotoinfo', array(
		'default'        => '',
		'sanitize_callback' => 'esc_textarea',
	) );
    $wp_customize->add_control( 'fulgent_mainmotoinfo', array(
		'label'   => __('Info','fulgent' ),
        'section' => 'fulgent_mainmoto_section',
        'type'    => 'textarea',
   ) );
   
	$wp_customize->add_setting( 'fulgent_breadcrumbs_image_bg',array(
		'sanitize_callback' => 'esc_url_raw',
		)
	);
    $wp_customize->add_control( new WP_Customize_Image_Control( $wp_customize, 'fulgent_breadcrumbs_image_bg', array(
			'label'    => __( 'Background Image (Recommended size 1280 x 853)', 'fulgent' ),
			'section'  => 'fulgent_mainmoto_section',
			'settings' => 'fulgent_breadcrumbs_image_bg',
		) 
	) );   
        
  
   // About Us
   $wp_customize->add_setting( 'fulgent_about_title', array(
		'default'        => '',
		'sanitize_callback' => 'esc_attr',
	) );
    $wp_customize->add_control( 'fulgent_about_title', array(
		'label'   => __('Title','fulgent' ),
		'section' => 'fulgent_aboutus_section',
		'type'    => 'text',
    ) );
    
    $wp_customize->add_setting( 'fulgent_aboutus_info', array(
		'default'        => '',
		'sanitize_callback' => 'esc_textarea',
	) );
    $wp_customize->add_control( 'fulgent_aboutus_info', array(
		'label'   => __('About Us Info','fulgent' ),
        'section' => 'fulgent_aboutus_section',
        'type'    => 'textarea',
	) );
	$wp_customize->add_setting( 'fulgent_aboutus_image_bg',array(
		'sanitize_callback' => 'esc_url_raw',
		)
	);
    $wp_customize->add_control( new WP_Customize_Image_Control( $wp_customize, 'fulgent_aboutus_image_bg', array(
			'label'    => __( 'Right Side Image (Recommended size 570 x 350)', 'fulgent' ),
			'section'  => 'fulgent_aboutus_section',
			'settings' => 'fulgent_aboutus_image_bg',
		) 
	) ); 
    $wp_customize->add_setting( 'fulgent_aboutus_right_text', array(
		'default'        => '',
		'sanitize_callback' => 'esc_textarea',
	) );
    $wp_customize->add_control( 'fulgent_aboutus_right_text', array(
		'label'   => __('Left Side Text','fulgent' ),
        'section' => 'fulgent_aboutus_section',
        'type'    => 'textarea',
	) );
       
       
     //Blog Section
	$wp_customize->add_setting( 'fulgent_blog_title', array(
		'default'        => '',
		'sanitize_callback' => 'esc_attr',
	) );
    
    $wp_customize->add_control( 'fulgent_blog_title', array(
		'label'   => __('Blog Title','fulgent' ),
        'section' => 'fulgent_blog_section',
        'type'    => 'text'
    ) );
    
  
        
	$fulgent_args = array(
	'posts_per_page'=> -1,
	'meta_query' => array(
						array(
						'key' => '_thumbnail_id',
						'compare' => 'EXISTS'
							),
						)
					);  
	$fulgent_post = new WP_Query( $fulgent_args );
	$fulgent_cat_id=array();
	while($fulgent_post->have_posts()){
	$fulgent_post->the_post();
	$fulgent_post_categories = wp_get_post_categories( get_the_id());
	foreach($fulgent_post_categories as $fulgent_post_category)
		$fulgent_cat_id[]=$fulgent_post_category;
	}
	wp_reset_postdata(); 
	
	$fulgent_cat_id=array_unique($fulgent_cat_id);
	$fulgent_args = array(
	'orderby' => 'name',
	'parent' => 0,
	'include'=>$fulgent_cat_id,
	
	);
	$fulgent_cats=array();$i = 0;
	$fulgent_categories = get_categories($fulgent_args); 
	  foreach ($fulgent_categories as $fulgent_category) {
		  if($i==0){
			$fulgent_default = $fulgent_category->term_id;
			$i++;
		}
		$fulgent_cats[$fulgent_category->term_id] =  $fulgent_category->cat_name;
	  }        
      
	 $wp_customize->add_setting( 'fulgent_blogcategory', array(
		'default'        => $fulgent_default,
		'sanitize_callback' => 'esc_attr',
				
	) );
    
    $wp_customize->add_control( 'fulgent_blogcategory', array(
			'label'   => __('Select Category','fulgent' ),
            'section' => 'fulgent_blog_section',
            'type'    => 'select',
            'choices' => $fulgent_cats,
        ) );
}
add_action( 'customize_register', 'fulgent_theme_customizer' );
?>
