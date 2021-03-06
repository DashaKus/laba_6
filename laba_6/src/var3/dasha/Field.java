package var3.dasha;

import java.awt.Color;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.util.ArrayList;
import javax.swing.JPanel;
import javax.swing.Timer;
@SuppressWarnings("serial")


public class Field extends JPanel {
    private boolean paused;
    private boolean magnetism;
    private ArrayList<BouncingBall> balls = new ArrayList<BouncingBall>(10);
    private Timer repaintTimer = new Timer(10, new ActionListener() {
        @Override
        public void actionPerformed(ActionEvent e) {
            repaint();
        }
    });
    // Конструктор класса BouncingBall
    public Field(){
        setBackground(Color.PINK);
        // Запустить таймер
        repaintTimer.start();
    }
    public void paintComponent(Graphics g){
        super.paintComponent(g);
        Graphics2D canvas = (Graphics2D) g;
        for(BouncingBall ball: balls){
            ball.paint(canvas);
        }
    }
    // Метод добавления нового мяча в список
    public void addBall(){
        balls.add(new BouncingBall(this));
    }

    public synchronized void pause(){
        paused=true;
        magnetism=false;
    }

    public synchronized void resume(){
        paused=false;
        magnetism=false;
        // Будим все ожидающие продолжения потоки
        notify();
    }
    // Синхронизированный метод проверки, может ли мяч двигаться
// (не включен ли режим паузы?)
    public synchronized void canMove(BouncingBall ball) throws
            InterruptedException{
        if (paused) {
            wait();
        }
    }
    public synchronized void magnetism(){
        magnetism = true;
        paused=false;
    }
}
