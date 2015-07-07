/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.gui.swing.basic;

import java.awt.Color;
import java.awt.Component;
import java.awt.Cursor;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.KeyboardFocusManager;
import java.awt.RenderingHints;
import java.awt.event.KeyEvent;
import java.awt.event.KeyListener;
import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;
import java.awt.font.FontRenderContext;
import java.awt.font.TextLayout;
import java.awt.geom.Rectangle2D;

import javax.swing.JComponent;
import javax.swing.JRootPane;
import javax.swing.SwingUtilities;


/**
 * Progress panel. 
 */
public class ProgressPanel extends JComponent implements MouseListener, KeyListener {

  private static final long serialVersionUID = -4675782468350775884L;

  private String application = "";
  private String text = "";
  private int    alphaLevel = 255;
  private float  shield = 0.70F;

  private RenderingHints hints = null;
  private Color background = Color.GRAY;
  private Color textBackground = Color.CYAN;
  private Color textBorder = Color.BLUE;

  private boolean started = false;
  private Component recentFocusOwner = null;

  /**
   * Constructor.
   */
  public ProgressPanel() {
    this(null);
  }

  /**
   * Constructor.
   * 
   * @param application Application name.
   */
  public ProgressPanel(String application) {
    this(application, null);
  }

  /**
   * Constructor.
   * 
   * @param application Application name.
   * @param text Displayed text.
   */
  public ProgressPanel(String application, String text) {
    this(application, text, 0.70F);
  }

  /**
   * Constructor.
   * 
   * @param application Application name.
   * @param text Displayed text.
   * @param shield Transparency.
   */
  public ProgressPanel(String application, String text, float shield) {
    this.application = application;
    this.text = text;
    this.shield = shield;

    this.hints = new RenderingHints(RenderingHints.KEY_RENDERING, RenderingHints.VALUE_RENDER_QUALITY);
    this.hints.put(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON);
    this.hints.put(RenderingHints.KEY_FRACTIONALMETRICS, RenderingHints.VALUE_FRACTIONALMETRICS_ON);

    this.background = new Color(255, 255, 255, (int) (alphaLevel * this.shield));
    this.textBackground = new Color(0, 255, 255, (int) (alphaLevel * this.shield));

    setOpaque(false);
    setCursor(Cursor.getPredefinedCursor(Cursor.WAIT_CURSOR));
  }

  /**
   * @param text Displayed text.
   */
  public void setText(String text) {
    this.text = text;
    repaint();
  }

  /**
   * @return Displayed text.
   */
  public String getText() {
    return text;
  }

  /**
   * Start progress panel.
   */
  public void start() {
    startPanel();
  }

  /**
   * Stop progress panel.
   */
  public void stop() {
    stopPanel();
  }

  /**
   * Interrupt progress panel.
   */
  public void interrupt() {
    stopPanel();
  }

  /**
   * Start progress panel. 
   */
  private void startPanel() {
    started = true;
    addMouseListener(this);
    addKeyListener(this);
    setVisible(true);
    repaint();
  }

  /**
   * Stop progress panel. 
   */
  private void stopPanel() {
    started = false;
    removeMouseListener(this);
    removeKeyListener(this);
    setText("");
    setVisible(false);
  }

  /* (non-Javadoc)
   * @see javax.swing.JComponent#setVisible(boolean)
   */
  @Override
  public void setVisible(boolean visible) {
    boolean oldVisible = isVisible();
    super.setVisible(visible);
    JRootPane rootPane = SwingUtilities.getRootPane(this);
    if ((rootPane != null) && (isVisible() != oldVisible)) {
      if (isVisible()) {
        Component focusOwner = KeyboardFocusManager.
            getCurrentKeyboardFocusManager().getPermanentFocusOwner();
        if ((focusOwner != null) &&
            SwingUtilities.isDescendingFrom(focusOwner, rootPane)) {
          recentFocusOwner = focusOwner;
        }
        rootPane.getLayeredPane().setVisible(false);
        requestFocusInWindow();
      } else {
        rootPane.getLayeredPane().setVisible(true);
        if (recentFocusOwner != null) {
          recentFocusOwner.requestFocusInWindow();
        }
        recentFocusOwner = null;
      }
    }
  }

  /* (non-Javadoc)
   * @see javax.swing.JComponent#paintComponent(java.awt.Graphics)
   */
  @Override
  public void paintComponent(Graphics g) {
    if (!started) {
      return;
    }

    int width = getWidth();
    int height = getHeight();

    // Draw layered pane below glass pane
    JRootPane rootPane = SwingUtilities.getRootPane(this);
    if (rootPane != null) {
      // It is important to call print() instead of paint() here
      // because print() doesn't affect the frame's double buffer
      rootPane.getLayeredPane().print(g);
    }
    // Draw background
    Graphics2D g2 = (Graphics2D) g;
    g2.setRenderingHints(hints);
    g2.setColor(background);
    g2.fillRect(0, 0, width, height);

    // Draw text
    if (((application != null) && (application.length() > 0)) ||
        ((text != null) && (text.length() > 0))) {

      // Positions
      float xMin = Float.MAX_VALUE;
      float xMax = Float.MIN_VALUE;
      float yMin = Float.MAX_VALUE;
      float yMax = Float.MIN_VALUE;
      float xSpace = 10F;
      float ySpace = 5F;

      // Compute bonds
      FontRenderContext context = g2.getFontRenderContext();
      TextLayout layoutApplication = null;
      float xApplication = Float.MAX_VALUE;
      float yApplication = Float.MIN_VALUE;
      if ((application != null) && (application.length() > 0)) {
        layoutApplication = new TextLayout(application, getFont(), context);
        Rectangle2D boundsApplication = layoutApplication.getBounds();
        xApplication = (float) (width - boundsApplication.getWidth()) / 2;
        yApplication = Math.max((float) ((float) height / 2 - 2 * boundsApplication.getHeight()), 0);
        xMin = Math.min(xMin, (float) (xApplication + boundsApplication.getMinX() - xSpace));
        xMax = Math.max(xMax, (float) (xApplication + boundsApplication.getMaxX() + xSpace));
        yMin = Math.min(yMin, (float) (yApplication + boundsApplication.getMinY() - ySpace));
        yMax = Math.max(yMax, (float) (yApplication + boundsApplication.getMaxY() + ySpace));
      }
      TextLayout layoutText = null;
      float xText = Float.MAX_VALUE;
      float yText = Float.MIN_VALUE;
      if ((text != null) && (text.length() > 0)) {
        layoutText = new TextLayout(text, getFont(), context);
        Rectangle2D boundsText = layoutText.getBounds();
        xText = (float) (width - boundsText.getWidth()) / 2;
        yText = Math.max((float) ((float) height / 2 + boundsText.getHeight()), 0);
        xMin = Math.min(xMin, (float) (xText + boundsText.getMinX() - xSpace));
        xMax = Math.max(xMax, (float) (xText + boundsText.getMaxX() + xSpace));
        yMin = Math.min(yMin, (float) (yText + boundsText.getMinY() - ySpace));
        yMax = Math.max(yMax, (float) (yText + boundsText.getMaxY() + ySpace));
      }

      // Draw border
      if ((xMin < xMax) && (yMin < yMax)) {
        g2.setColor(textBackground);
        g2.fillRoundRect((int) xMin, (int) yMin, (int) (xMax - xMin), (int) (yMax - yMin), 10, 10);
        g2.setColor(textBorder);
        g2.drawRoundRect((int) xMin, (int) yMin, (int) (xMax - xMin), (int) (yMax - yMin), 10, 10);
      }

      // Draw text
      g2.setColor(getForeground());
      if (layoutApplication != null) {
        layoutApplication.draw(g2, xApplication, yApplication);
      }
      if (layoutText != null) {
        layoutText.draw(g2, xText, yText);
      }
    }
  }

  /* ====================================================================== */
  /* MouseListener implementation                                           */
  /* ====================================================================== */

  /* (non-Javadoc)
   * @see java.awt.event.MouseListener#mouseClicked(java.awt.event.MouseEvent)
   */
  @Override
  public void mouseClicked(@SuppressWarnings("unused") MouseEvent e) {
    //
  }

  /* (non-Javadoc)
   * @see java.awt.event.MouseListener#mouseEntered(java.awt.event.MouseEvent)
   */
  @Override
  public void mouseEntered(@SuppressWarnings("unused") MouseEvent e) {
    //
  }

  /* (non-Javadoc)
   * @see java.awt.event.MouseListener#mouseExited(java.awt.event.MouseEvent)
   */
  @Override
  public void mouseExited(@SuppressWarnings("unused") MouseEvent e) {
    //
  }

  /* (non-Javadoc)
   * @see java.awt.event.MouseListener#mousePressed(java.awt.event.MouseEvent)
   */
  @Override
  public void mousePressed(@SuppressWarnings("unused") MouseEvent e) {
    //
  }

  /* (non-Javadoc)
   * @see java.awt.event.MouseListener#mouseReleased(java.awt.event.MouseEvent)
   */
  @Override
  public void mouseReleased(@SuppressWarnings("unused") MouseEvent e) {
    //
  }

  /* ====================================================================== */
  /* KeyListener implementation                                             */
  /* ====================================================================== */

  /* (non-Javadoc)
   * @see java.awt.event.KeyListener#keyPressed(java.awt.event.KeyEvent)
   */
  @Override
  public void keyPressed(KeyEvent e) {
    e.consume();
  }

  /* (non-Javadoc)
   * @see java.awt.event.KeyListener#keyReleased(java.awt.event.KeyEvent)
   */
  @Override
  public void keyReleased(KeyEvent e) {
    e.consume();
  }

  /* (non-Javadoc)
   * @see java.awt.event.KeyListener#keyTyped(java.awt.event.KeyEvent)
   */
  @Override
  public void keyTyped(KeyEvent e) {
    e.consume();
  }
}
