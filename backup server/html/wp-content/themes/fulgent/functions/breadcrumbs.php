<?php
/*
 * fulgent Breadcrumbs
*/
function fulgent_custom_breadcrumbs() {

  $fulgent_showonhome = 0; // 1 - show breadcrumbs on the homepage, 0 - don't show
  $fulgent_delimiter = '/'; // fulgent_delimiter between crumbs
  $fulgent_home = __('Home','fulgent'); // text for the 'Home' link
  $fulgent_showcurrent = 1; // 1 - show current post/page title in breadcrumbs, 0 - don't show
  $fulgent_before = ' '; // tag before the current crumb
  $fulgent_after = ' '; // tag after the current crumb

  global $post;
  $fulgent_homelink = esc_url(home_url('/'));

  if (is_home() || is_front_page()) {

    if ($fulgent_showonhome == 1) echo '<li><a href="' . $fulgent_homelink . '">' . $fulgent_home . '</a></li>';
    
  }  else {

    echo '<li><a href="' . $fulgent_homelink . '">' . $fulgent_home . '</a> ' . $fulgent_delimiter . '';
    
   if ( is_category() ) {
      $fulgent_thisCat = get_category(get_query_var('cat'), false);
      if ($fulgent_thisCat->parent != 0) echo get_category_parents($fulgent_thisCat->parent, TRUE, ' ' . $fulgent_delimiter . ' ');      
		echo $fulgent_before; _e('category','fulgent'); echo ' "'.single_cat_title('', false) . '"' . $fulgent_after;
    } 
    elseif ( is_search() ) {
      echo $fulgent_before; _e('Search Results For','fulgent'); echo ' "'. get_search_query() . '"' . $fulgent_after;

    } elseif ( is_day() ) {
      echo '<a href="' . esc_url(get_year_link(get_the_time('Y'))) . '">' . esc_attr(get_the_time('Y')) . '</a> ' . $fulgent_delimiter . ' ';
      echo '<a href="' . esc_url(get_month_link(get_the_time('Y'),get_the_time('m'))) . '">' . esc_attr(get_the_time('F')) . '</a> ' . $fulgent_delimiter . ' ';
      echo $fulgent_before . get_the_time('d') . $fulgent_after;

    } elseif ( is_month() ) {
      echo '<a href="' . esc_url(get_year_link(get_the_time('Y'))) . '">' . esc_attr(get_the_time('Y')) . '</a> ' . $fulgent_delimiter . ' ';
      echo $fulgent_before . get_the_time('F') . $fulgent_after;

    } elseif ( is_year() ) {
      echo $fulgent_before . get_the_time('Y') . $fulgent_after;

    } elseif ( is_single() && !is_attachment() ) {
      if ( get_post_type() != 'post' ) {
        $fulgent_post_type = get_post_type_object(get_post_type());
        $fulgent_slug = $fulgent_post_type->rewrite;
        echo '<a href="' . $fulgent_homelink . '/' . $fulgent_slug['slug'] . '/">' . $fulgent_post_type->labels->singular_name . '</a>';
        if ($fulgent_showcurrent == 1) echo ' ' . $fulgent_delimiter . ' ' . $fulgent_before . esc_attr(get_the_title()) . $fulgent_after;
      } else {
        $fulgent_cat = get_the_category(); $fulgent_cat = $fulgent_cat[0];
        $fulgent_cats = get_category_parents($fulgent_cat, TRUE, ' ' . $fulgent_delimiter . ' ');
        if ($fulgent_showcurrent == 0) $fulgent_cats = preg_replace("#^(.+)\s$fulgent_delimiter\s$#", "$1", $fulgent_cats);
        echo $fulgent_cats;
        if ($fulgent_showcurrent == 1) echo $fulgent_before . esc_attr(get_the_title()) . $fulgent_after;
      }

    } elseif ( !is_single() && !is_page() && get_post_type() != 'post' && !is_404() ) {
      $fulgent_post_type = get_post_type_object(get_post_type());
      echo $fulgent_before . $fulgent_post_type->labels->singular_name . $fulgent_after;

    } elseif ( is_attachment() ) {
      $fulgent_parent = get_post($post->post_parent);
      $fulgent_cat = get_the_category($fulgent_parent->ID); $fulgent_cat = $fulgent_cat[0];
      echo get_category_parents($fulgent_cat, TRUE, ' ' . $fulgent_delimiter . ' ');
      echo '<a href="' . esc_url(get_permalink($fulgent_parent)) . '">' . $fulgent_parent->post_title . '</a>';
      if ($fulgent_showcurrent == 1) echo ' ' . $fulgent_delimiter . ' ' . $fulgent_before . esc_attr(get_the_title()) . $fulgent_after;

    } elseif ( is_page() && !$post->post_parent ) {
      if ($fulgent_showcurrent == 1) echo $fulgent_before . esc_attr(get_the_title()) . $fulgent_after;

    } elseif ( is_page() && $post->post_parent ) {
      $fulgent_parent_id  = $post->post_parent;
      $fulgent_breadcrumbs = array();
      while ($fulgent_parent_id) {
        $fulgent_page = get_post($fulgent_parent_id);
        $fulgent_breadcrumbs[] = '<a href="' . esc_url(get_permalink($fulgent_page)) . '">' . esc_attr(get_the_title($fulgent_page)) . '</a>';
        $fulgent_parent_id  = $fulgent_page->post_parent;
      }
      $fulgent_breadcrumbs = array_reverse($fulgent_breadcrumbs);
      for ($fulgent_i = 0; $fulgent_i < count($fulgent_breadcrumbs); $fulgent_i++) {
        echo $fulgent_breadcrumbs[$fulgent_i];
        if ($fulgent_i != count($fulgent_breadcrumbs)-1) echo ' ' . $fulgent_delimiter . ' ';
      }
      if ($fulgent_showcurrent == 1) echo ' ' . $fulgent_delimiter . ' ' . $fulgent_before . esc_attr(get_the_title()) . $fulgent_after;

    } elseif ( is_tag() ) {
      echo $fulgent_before; _e('Posts tagged','fulgent'); echo ' "'.  single_tag_title('', false) . '"' . $fulgent_after;

    } elseif ( is_author() ) {
       global $author;
      $fulgent_userdata = get_userdata($author);
      echo $fulgent_before; _e('Articles posted by ','fulgent'); echo esc_attr($fulgent_userdata->display_name) . $fulgent_after;

    } elseif ( is_404() ) {
      echo $fulgent_before; _e('Error 404','fulgent'); echo $fulgent_after;
    }
    
    if ( get_query_var('paged') ) {
      if ( is_category() || is_day() || is_month() || is_year() || is_search() || is_tag() || is_author() ) echo ' (';
      echo __('Page','fulgent') . ' ' . get_query_var('paged');
      if ( is_category() || is_day() || is_month() || is_year() || is_search() || is_tag() || is_author() ) echo ')';
    }
    echo '</li>';

  }
} 
