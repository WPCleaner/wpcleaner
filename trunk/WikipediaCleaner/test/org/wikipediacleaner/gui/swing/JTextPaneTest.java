/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.gui.swing;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Component;
import java.awt.Dimension;

import javax.swing.JFrame;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JTextPane;
import javax.swing.ScrollPaneConstants;
import javax.swing.SwingUtilities;
import javax.swing.UIManager;
import javax.swing.UIManager.LookAndFeelInfo;
import javax.swing.UnsupportedLookAndFeelException;


/**
 * Simple test with JTextPane
 */
public class JTextPaneTest extends JPanel {

  private static final long serialVersionUID = 6426424812124490870L;

  private JTextPane textNewSection;

  public JTextPaneTest() {
    setLayout(new BorderLayout());
    add(createTextPane(), BorderLayout.CENTER);
  }

  private Component createTextPane() {
    textNewSection = new JTextPane();
    textNewSection.setBackground(Color.WHITE);
    textNewSection.setEditable(true);
    JScrollPane scrollContents = new JScrollPane(textNewSection);
    scrollContents.setMinimumSize(new Dimension(100, 100));
    scrollContents.setPreferredSize(new Dimension(1000, 500));
    scrollContents.setVerticalScrollBarPolicy(ScrollPaneConstants.VERTICAL_SCROLLBAR_ALWAYS);
    return scrollContents;
  }

  static void createAndShowGui() {
    JFrame frame = new JFrame("JTextPane test");
    frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
    frame.add(new JTextPaneTest());
    frame.pack();
    frame.setVisible(true);
  }

  public static void main(String[] args) {
    SwingUtilities.invokeLater(new Runnable() {
      
      @Override
      public void run() {
        try {
          LookAndFeelInfo[] infos = UIManager.getInstalledLookAndFeels();
          for (int i = 0; i < infos.length; i++) {
            System.out.println(infos[i].getClassName());
          }
          System.out.println(System.getProperty("java.version"));
          System.out.println(UIManager.getSystemLookAndFeelClassName());
          UIManager.setLookAndFeel(UIManager.getSystemLookAndFeelClassName());
        } catch (ClassNotFoundException e) {
          // TODO
        } catch (InstantiationException e) {
          // TODO
        } catch (IllegalAccessException e) {
          // TODO
        } catch (UnsupportedLookAndFeelException e) {
          // TODO
        }
        createAndShowGui();
      }
    });
  }
}
