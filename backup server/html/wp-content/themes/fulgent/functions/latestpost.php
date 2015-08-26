<?php
/*
 * fulgent Popular Post
*/
add_action( 'widgets_init', 'fulgent_widget_post' );

function fulgent_widget_post() {
    register_widget( 'fulgent_LatestPostWidget' );
}

class fulgent_LatestPostWidget extends WP_Widget
{
function fulgent_LatestPostWidget()
{
$fulgent_lp_widget_ops = array('classname' => 'LatestPostWidget', 'description' => __('Displays a Latest post with thumbnail','fulgent') );
$this->WP_Widget('LatestPostWidget', __('fulgent Latest Post', 'fulgent'), $fulgent_lp_widget_ops);
}

function form($fulgent_lp_instance)
{
$fulgent_lp_instance = wp_parse_args( (array) $fulgent_lp_instance, array( 'title' => '' ) );
$fulgent_lp_title = $fulgent_lp_instance['title'];
$fulgent_lp_number = isset( $fulgent_lp_instance['number'] ) ? absint( $fulgent_lp_instance['number'] ) : 5;
$fulgent_lp_show_date = isset( $fulgent_lp_instance['show_date'] ) ? (bool) $fulgent_lp_instance['show_date'] : false; 
?>
<p><label for="<?php echo $this->get_field_id('title'); ?>"><?php _e( 'Title:', 'fulgent' ); ?> <input id="<?php echo $this->get_field_id('title'); ?>" name="<?php echo $this->get_field_name('title'); ?>" type="text" value="<?php echo esc_attr($fulgent_lp_title); ?>" /></label></p>
 <p><label for="<?php echo $this->get_field_id( 'number' ); ?>"><?php _e( 'Number of posts to show:', 'fulgent' ); ?></label>
<input id="<?php echo $this->get_field_id( 'number' ); ?>" maxlength="2" name="<?php echo $this->get_field_name( 'number' ); ?>" type="text" value="<?php echo $fulgent_lp_number; ?>" size="3" /></p> 
 <p><input class="checkbox" type="checkbox" <?php checked( $fulgent_lp_show_date ); ?> id="<?php echo $this->get_field_id( 'show_date' ); ?>" name="<?php echo $this->get_field_name( 'show_date' ); ?>" />
 <label for="<?php echo $this->get_field_id( 'show_date' ); ?>"><?php _e( 'Display post date?', 'fulgent' ); ?></label></p>
<?php
}

function update($fulgent_lp_new_instance, $fulgent_lp_old_instance)
{
  $fulgent_lp_instance = $fulgent_lp_old_instance;
        $fulgent_lp_instance['title'] = sanitize_text_field(strip_tags($fulgent_lp_new_instance['title']));
        $fulgent_lp_instance['number'] = (int) $fulgent_lp_new_instance['number']; 
       
        $fulgent_lp_instance['show_date'] = (bool) $fulgent_lp_new_instance['show_date'];
		return $fulgent_lp_instance;

}

function widget($args, $fulgent_lp_instance)
{
extract($args, EXTR_SKIP);

echo $before_widget;
$fulgent_lp_title = empty($fulgent_lp_instance['title']) ? ' ' : apply_filters('widget_title', $fulgent_lp_instance['title']);

if (!empty($fulgent_lp_title))
echo $before_title . $fulgent_lp_title . $after_title;;
  $fulgent_lp_show_date = isset( $fulgent_lp_instance['show_date'] ) ? $fulgent_lp_instance['show_date'] : false;
  $fulgent_lp_number = ( ! empty( $fulgent_lp_instance['number'] ) ) ? absint( $fulgent_lp_instance['number'] ) : 10;


$query = new WP_Query( apply_filters( 'widget_posts_args', 
					array( 'posts_per_page' => $fulgent_lp_number, 
							'no_found_rows' => true, 
							'post_status' => 'publish', 
							'ignore_sticky_posts' => true,
								'meta_query' => array(
							array(
							 'key' => '_thumbnail_id',
							 'compare' => 'EXISTS'
							),
						)	
						) ) );?>
<div class="popular-posts">
<ul class="posts">
		<?php if ($query->have_posts()) :
			while ( $query->have_posts() ) : $query->the_post(); ?>
			<li>
				<span class="thumb">
					<?php if ( has_post_thumbnail() ) : ?>
						<a href="<?php echo esc_url( get_permalink() ); ?>">
							<?php the_post_thumbnail( 'fulgent-latest-posts-widget', array( 'alt' => get_the_title(), 'class' => 'img-responsive') ); ?>
						</a>
					<?php endif; ?>
				</span>
				<p>
					<a href="<?php the_permalink() ?>" title="<?php echo  get_the_title() ? get_the_title() : get_the_ID(); ?>"><?php if ( get_the_title() ) the_title(); else the_ID(); ?></a> 
					<?php if ( $fulgent_lp_show_date ) : ?>          
						<span><?php echo get_the_date(); ?></span>
					<?php endif; ?>
				</p>	
          </li>
       <?php endwhile; endif; // end of the loop.?>
      <?php wp_reset_postdata() ?>
</ul>
</div>
<?php echo $after_widget;
}

}
add_action( 'widgets_init', create_function('', 'return register_widget("fulgent_LatestPostWidget");') );
?>
