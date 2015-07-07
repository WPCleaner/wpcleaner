/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.gui.swing.component;

import java.awt.event.KeyEvent;
import java.awt.event.KeyListener;
import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;


/**
 * A base class for all popup listeners.
 */
public abstract class AbstractPopupListener implements MouseListener, KeyListener {

  // ==========================================================================
  // KeyListener methods
  // ==========================================================================

  /**
   * Show popup menu in response to a key event.
   * 
   * @param e Event.
   */
  protected abstract void showPopup(KeyEvent e);

  /**
   * Check conditions for displaying a popup menu in response to a key event.
   * 
   * @param e Event.
   */
  private void maybeShowPopup(KeyEvent e) {
    if (e == null) {
      return;
    }
    if (e.getKeyCode() != KeyEvent.VK_CONTEXT_MENU) {
      return;
    }
    showPopup(e);
  }

  /**
   * Invoked when a key has been typed.
   * 
   * @param e Event.
   * @see java.awt.event.KeyListener#keyTyped(java.awt.event.KeyEvent)
   */
  @Override
  public void keyTyped(KeyEvent e) {
    // Nothing to do
  }

  /**
   * Invoked when a key has been pressed. 
   * 
   * @param e Event.
   * @see java.awt.event.KeyListener#keyPressed(java.awt.event.KeyEvent)
   */
  @Override
  public void keyPressed(KeyEvent e) {
    // Nothing to do
  }

  /**
   * Invoked when a key has been released.
   * 
   * @param e Event.
   * @see java.awt.event.KeyListener#keyReleased(java.awt.event.KeyEvent)
   */
  @Override
  public void keyReleased(KeyEvent e) {
    maybeShowPopup(e);
  }

  // ==========================================================================
  // MouseListener methods
  // ==========================================================================

  /**
   * Show popup menu in response to a mouse event.
   * 
   * @param e Event.
   */
  protected abstract void showPopup(MouseEvent e);

  /**
   * Check conditions for displaying a popup menu in response to a mouse event.
   * 
   * @param e Event.
   */
  private void maybeShowPopup(MouseEvent e) {
    if (e == null) {
      return;
    }
    if (!e.isPopupTrigger()) {
      return;
    }
    showPopup(e);
  }

  /**
   * Invoked when the mouse button has been clicked (pressed
   * and released) on a component.
   * 
   * @param e Event.
   * @see java.awt.event.MouseListener#mouseClicked(java.awt.event.MouseEvent)
   */
  @Override
  public void mouseClicked(MouseEvent e) {
    maybeShowPopup(e);
  }

  /**
   * Invoked when a mouse button has been pressed on a component.
   * 
   * @param e Event.
   * @see java.awt.event.MouseListener#mousePressed(java.awt.event.MouseEvent)
   */
  @Override
  public void mousePressed(MouseEvent e) {
    maybeShowPopup(e);
  }

  /**
   * Invoked when a mouse button has been released on a component.
   * 
   * @param e Event.
   * @see java.awt.event.MouseListener#mouseReleased(java.awt.event.MouseEvent)
   */
  @Override
  public void mouseReleased(MouseEvent e) {
    maybeShowPopup(e);
  }

  /**
   * Invoked when the mouse enters a component.
   * 
   * @param e Event.
   * @see java.awt.event.MouseListener#mouseEntered(java.awt.event.MouseEvent)
   */
  @Override
  public void mouseEntered(MouseEvent e) {
    // Nothing to do
  }

  /**
   * Invoked when the mouse exits a component.
   * 
   * @param e Event.
   * @see java.awt.event.MouseListener#mouseExited(java.awt.event.MouseEvent)
   */
  @Override
  public void mouseExited(MouseEvent e) {
    // Nothing to do
  }

}
