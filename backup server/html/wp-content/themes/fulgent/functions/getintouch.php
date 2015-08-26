<?php
/*
 * fulgent Get in touch
*/
add_action( 'widgets_init', 'fulgent_widget' );
function fulgent_widget() {
    register_widget( 'fulgent_info_widget' );
}
class fulgent_info_widget extends WP_Widget {
    function fulgent_info_widget() {
        $fulgent_widget_ops = array( 'classname' => 'fulgent_info' , 'description' => __('A widget that displays the title,address,phone,fax,email,website information. ','fulgent') );
        $fulgent_control_ops = array( 'width' => 300, 'height' => 350, 'id_base' => 'fulgent-info-widget' );
        $this->WP_Widget( 'fulgent-info-widget', __('Get In Touch', 'fulgent'), $fulgent_widget_ops, $fulgent_control_ops );
    }
    function widget( $fulgent_args, $fulgent_instance ) {
        extract( $fulgent_args );
        //Our variables from the widget settings.
        $fulgent_title = apply_filters('widget_title', $fulgent_instance['title'] );
        $fulgent_address = sanitize_text_field(strip_tags($fulgent_instance['address']));
        $fulgent_phone = sanitize_text_field(strip_tags($fulgent_instance['phone']));
        $fulgent_fax = sanitize_text_field(strip_tags($fulgent_instance['fax']));
        $fulgent_email = sanitize_email(strip_tags($fulgent_instance['email']));
        $fulgent_website = esc_url_raw(strip_tags($fulgent_instance['website']));

        echo $before_widget;

        //Display widget
?>
<h3 class="widget-title"><?php if(!empty($fulgent_instance['title'])){ echo esc_attr($fulgent_instance['title']); } else { echo ""._e('Contributor','fulgent').""; }?></h3>
  <div class="contct-widget">
	<?php if(!empty($fulgent_instance['address'])) { ?>
		<?php echo '<p>'.esc_attr($fulgent_instance['address']).'</p>'; ?>
    <?php } ?>
    <?php if(!empty($fulgent_instance['phone'])) { ?>
        <p><?php _e('Phone:','fulgent');?> <?php echo esc_attr($fulgent_instance['phone']); ?></p>
    <?php } ?>

    <?php if(!empty($fulgent_instance['fax'])) { ?>
        <p><?php _e('Fax:','fulgent');?> <?php echo esc_attr($fulgent_instance['fax']); ?></p>
    <?php } ?>

    <?php if(!empty($fulgent_instance['email'])) { ?>
        <p><?php _e('Email:','fulgent');?> <a href="mailto:<?php echo sanitize_email($fulgent_instance['email']); ?>"><?php echo sanitize_email($fulgent_instance['email']); ?></a></p>
    <?php } ?>

    <?php if(!empty($fulgent_instance['website'])) { ?>
        <p><?php _e('Web:','fulgent');?><a href="<?php echo esc_url($fulgent_instance['website']); ?>"><?php echo esc_url($fulgent_instance['website']); ?></a></p>
    <?php } ?>
          </div>
  
<?php        
        echo $after_widget;
    }
    //Update the widget
    function update( $new_instance, $old_instance ) {
        $fulgent_instance = $old_instance;

        //Strip tags from title and name to remove HTML
        $fulgent_instance['title'] = strip_tags( $new_instance['title'] );
        $fulgent_instance['address'] = strip_tags( $new_instance['address'] );
        $fulgent_instance['country'] = strip_tags( $new_instance['country'] );	
        $fulgent_instance['phone'] = strip_tags( $new_instance['phone'] );
        $fulgent_instance['fax'] = strip_tags( $new_instance['fax'] );
        $fulgent_instance['website'] = esc_url_raw(strip_tags( $new_instance['website']));
        $fulgent_instance['email'] = sanitize_email(strip_tags( $new_instance['email'] ));
		
        return $fulgent_instance;
    }

    function form( $fulgent_instance ) {
?>
<p>
  <label for="<?php echo $this->get_field_id( 'title' ); ?>">
    <?php _e('Widget Title:', 'fulgent'); ?>
  </label>
  <input id="<?php echo $this->get_field_id( 'title' ); ?>" name="<?php echo $this->get_field_name( 'title' ); ?>" value="<?php if(!empty($fulgent_instance['title'])) { echo $fulgent_instance['title']; } ?>"  type="text" class="widefat" />
</p>
<p>
  <label for="<?php echo $this->get_field_id( 'address' ); ?>">
    <?php _e('Address:', 'fulgent'); ?>
  </label>
  <textarea id="<?php echo $this->get_field_id( 'address' ); ?>" name="<?php echo $this->get_field_name( 'address' ); ?>"  class="widefat" ><?php if(!empty($fulgent_instance['address'])) { echo esc_textarea($fulgent_instance['address']); } ?> </textarea> 
</p>

<p>
  <label for="<?php echo $this->get_field_id( 'phone' ); ?>">
    <?php _e('Phone:', 'fulgent'); ?>
  </label>
  <input id="<?php echo $this->get_field_id( 'phone' ); ?>" name="<?php echo $this->get_field_name( 'phone' ); ?>" value="<?php if(!empty($fulgent_instance['phone'])) { echo esc_attr($fulgent_instance['phone']); } ?>" type="text" class="widefat" />
</p>
<p>
  <label for="<?php echo $this->get_field_id( 'fax' ); ?>">
    <?php _e('Fax:', 'fulgent'); ?>
  </label>
  <input id="<?php echo $this->get_field_id( 'fax' ); ?>" name="<?php echo $this->get_field_name( 'fax' ); ?>" value="<?php if(!empty($fulgent_instance['fax'])) { echo esc_attr($fulgent_instance['fax']); } ?>" type="text" class="widefat" />
</p>
<p>
  <label for="<?php echo $this->get_field_id( 'email' ); ?>">
    <?php _e('Email Address:', 'fulgent'); ?>
  </label>
  <input id="<?php echo $this->get_field_id( 'email' ); ?>" name="<?php echo $this->get_field_name( 'email' ); ?>" value="<?php if(!empty($fulgent_instance['email'])) { echo esc_attr($fulgent_instance['email']); } ?>" type="text" class="widefat" />
</p>
<p>
  <label for="<?php echo $this->get_field_id( 'website' ); ?>">
    <?php _e('Website:', 'fulgent'); ?>
  </label>
  <input id="<?php echo $this->get_field_id( 'website' ); ?>" name="<?php echo $this->get_field_name( 'website' ); ?>" value="<?php if(!empty($fulgent_instance['website'])) { echo esc_attr($fulgent_instance['website']); } ?>" type="text" class="widefat" />
</p>

<?php
    }
}

?>
