/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.gui.swing.options;

import java.awt.LayoutManager;
import java.util.Enumeration;
import java.util.HashMap;
import java.util.Vector;
import java.util.Map.Entry;

import javax.swing.AbstractButton;
import javax.swing.ButtonGroup;
import javax.swing.JCheckBox;
import javax.swing.JComboBox;
import javax.swing.JComponent;
import javax.swing.JPanel;
import javax.swing.JSpinner;
import javax.swing.JTextField;
import javax.swing.SpinnerModel;
import javax.swing.SpinnerNumberModel;

import org.wikipediacleaner.gui.swing.basic.Utilities;
import org.wikipediacleaner.utils.Configuration;
import org.wikipediacleaner.utils.ConfigurationValueBoolean;
import org.wikipediacleaner.utils.ConfigurationValueInteger;
import org.wikipediacleaner.utils.ConfigurationValueString;


/**
 * Base class for an Options panel.
 */
abstract class OptionsPanel extends JPanel {

  /**
   * Serialization.
   */
  private static final long serialVersionUID = -6581007549837818171L;

  /**
   * Construct an Options panel.
   * 
   * @param layout Layout manager.
   */
  public OptionsPanel(LayoutManager layout) {
    super(layout);
    booleanValues = new HashMap<ConfigurationValueBoolean, JCheckBox>();
    integerValues = new HashMap<ConfigurationValueInteger, Object>();
    stringValues = new HashMap<ConfigurationValueString, JComponent>();
  }

  // ==========================================================================
  // Actions management
  // ==========================================================================

  /**
   * Restore all options to their default values.
   */
  public void defaultValues() {
    defaultValuesBoolean();
    defaultValuesInteger();
    defaultValuesString();
  }

  /**
   * Apply new values to the options.
   */
  public void apply() {
    applyBoolean();
    applyInteger();
    applyString();
  }

  // ==========================================================================
  // Boolean options management
  // ==========================================================================

  /**
   * Map of check box for each boolean property.
   */
  private final HashMap<ConfigurationValueBoolean, JCheckBox> booleanValues;

  /**
   * @param message Message displayed in the JCheckBox.
   * @param property Boolean property.
   * @return JCheckBox for the boolean property.
   */
  protected JCheckBox createJCheckBox(
      String message,
      ConfigurationValueBoolean property) {
    if (property == null) {
      return null;
    }
    Configuration config = Configuration.getConfiguration();
    boolean selected = config.getBoolean(null, property);
    JCheckBox chk = Utilities.createJCheckBox(message, selected);
    booleanValues.put(property, chk);
    return chk;
  }

  /**
   * Restore all boolean options to their default values.
   */
  private void defaultValuesBoolean() {
    for (Entry<ConfigurationValueBoolean, JCheckBox> entry : booleanValues.entrySet()) {
      if ((entry.getValue() != null) && (entry.getKey() != null)) {
        entry.getValue().setSelected(entry.getKey().getDefaultValue());
      }
    }
  }

  /**
   * Apply new values to the boolean options.
   */
  private void applyBoolean() {
    Configuration config = Configuration.getConfiguration();

    for (Entry<ConfigurationValueBoolean, JCheckBox> entry : booleanValues.entrySet()) {
      if ((entry.getValue() != null) && (entry.getKey() != null)) {
        config.setBoolean(null, entry.getKey(), entry.getValue().isSelected());
      }
    }
  }

  // ==========================================================================
  // Integer options management
  // ==========================================================================

  /**
   * Map of spinner for each integer property.
   */
  private final HashMap<ConfigurationValueInteger, Object> integerValues;

  /**
   * @param property Integer property.
   * @param minimum Minimum value.
   * @param maximum Maximum value.
   * @param stepSize Step size.
   * @return JSpinner for the integer property.
   */
  protected JSpinner createJSpinner(
      ConfigurationValueInteger property,
      int minimum, int maximum, int stepSize) {
    if (property == null) {
      return null;
    }
    Configuration config = Configuration.getConfiguration();
    int value = config.getInt(null, property);
    value = Math.max(minimum, Math.min(maximum, value));
    SpinnerNumberModel model = new SpinnerNumberModel(value, minimum, maximum, stepSize);
    JSpinner spin = new JSpinner(model);
    integerValues.put(property, spin);
    return spin;
  }

  /**
   * @param property Integer property.
   */
  protected void setButtonGroup(
      ConfigurationValueInteger property,
      ButtonGroup group) {
    if ((property == null) || (group == null)) {
      return;
    }
    Configuration config = Configuration.getConfiguration();
    int value = config.getInt(null, property);
    integerValues.put(property, group);
    setButtonGroupSelection(group, value);
  }

  /**
   * Restore all integer options to their default values.
   */
  private void defaultValuesInteger() {
    for (Entry<ConfigurationValueInteger, Object> entry : integerValues.entrySet()) {
      if ((entry.getValue() != null) && (entry.getKey() != null)) {
        if (entry.getValue() instanceof JSpinner) {
          JSpinner spinner = (JSpinner) entry.getValue();
          SpinnerModel model = spinner.getModel();
          model.setValue(Integer.valueOf(entry.getKey().getDefaultValue()));
        }
        if (entry.getValue() instanceof ButtonGroup) {
          ButtonGroup group = (ButtonGroup) entry.getValue();
          setButtonGroupSelection(group, entry.getKey().getDefaultValue());
        }
      }
    }
  }

  /**
   * @param group Button group.
   * @param value Value.
   */
  private void setButtonGroupSelection(ButtonGroup group, int value) {
    if (group == null) {
      return;
    }
    Enumeration<AbstractButton> buttons = group.getElements();
    int count = 0;
    while (buttons.hasMoreElements()) {
      AbstractButton button = buttons.nextElement();
      group.setSelected(button.getModel(), (count == value));
      count++;
    }
  }

  /**
   * Apply new values to the integer options.
   */
  private void applyInteger() {
    Configuration config = Configuration.getConfiguration();

    for (Entry<ConfigurationValueInteger, Object> entry : integerValues.entrySet()) {
      if ((entry.getValue() != null) && (entry.getKey() != null)) {
        if (entry.getValue() instanceof JSpinner) {
          JSpinner spinner = (JSpinner) entry.getValue();
          Object value = spinner.getValue();
          if (value instanceof Integer) {
            Integer intValue = (Integer) value;
            config.setInt(null, entry.getKey(), intValue.intValue());
          }
        }
        if (entry.getValue() instanceof ButtonGroup) {
          ButtonGroup group = (ButtonGroup) entry.getValue();
          int count = 0;
          Enumeration<AbstractButton> buttons = group.getElements();
          while (buttons.hasMoreElements()) {
            AbstractButton button = buttons.nextElement();
            if (group.isSelected(button.getModel())) {
              config.setInt(null, entry.getKey(), count);
            }
            count++;
          }
        }
      }
    }
  }

  // ==========================================================================
  // String options management
  // ==========================================================================

  /**
   * Map of text field for each string property.
   */
  private final HashMap<ConfigurationValueString, JComponent> stringValues;

  /**
   * @param property String property.
   * @param columns Number of columns in the text field.
   * @return JTextField for the string property.
   */
  protected JTextField createJTextField(
      ConfigurationValueString property,
      int columns) {
    if (property == null) {
      return null;
    }
    Configuration config = Configuration.getConfiguration();
    String value = config.getString(null, property);
    JTextField txt = new JTextField(columns);
    txt.setText(value);
    stringValues.put(property, txt);
    return txt;
  }

  protected JComboBox createJComboBox(
      ConfigurationValueString property,
      Vector<String> items) {
    if (property == null) {
      return null;
    }
    JComboBox<String> combo = new JComboBox<String>(items);
    combo.setEditable(false);
    Configuration config = Configuration.getConfiguration();
    String value = config.getString(null, property);
    combo.setSelectedItem(value);
    stringValues.put(property, combo);
    return combo;
  }

  /**
   * Restore all string options to their default values.
   */
  private void defaultValuesString() {
    for (Entry<ConfigurationValueString, JComponent> entry : stringValues.entrySet()) {
      if ((entry.getValue() != null) && (entry.getKey() != null)) {
        if (entry.getValue() instanceof JTextField) {
          JTextField text = (JTextField) entry.getValue();
          text.setText(entry.getKey().getDefaultValue());
        }
        if (entry.getValue() instanceof JComboBox) {
          JComboBox combo = (JComboBox) entry.getValue();
          combo.setSelectedItem(entry.getKey().getDefaultValue());
        }
      }
    }
  }

  /**
   * Apply new values to the string options.
   */
  private void applyString() {
    Configuration config = Configuration.getConfiguration();

    for (Entry<ConfigurationValueString, JComponent> entry : stringValues.entrySet()) {
      if ((entry.getValue() != null) && (entry.getKey() != null)) {
        if (entry.getValue() instanceof JTextField) {
          JTextField text = (JTextField) entry.getValue();
          config.setString(null, entry.getKey(), text.getText());
        }
        if (entry.getValue() instanceof JComboBox) {
          JComboBox combo = (JComboBox) entry.getValue();
          Object selection = combo.getSelectedItem();
          if (selection != null) {
            config.setString(null, entry.getKey(), selection.toString());
          }
        }
      }
    }
  }
}
