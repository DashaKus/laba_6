package var3.dasha;

import java.awt.Color;
import java.awt.Graphics2D;
import java.awt.geom.Ellipse2D;

public class BouncingBall implements Runnable {
    private static final int MAX_RADIUS =40;
    private static final int MIN_RADIUS=3;
    private static final int MAX_SPEED =15;

    private Field field;
    private int radius;
    private Color color;

    // Текущие координаты мяча
    private double x;
    private double y;

    // Вертикальная и горизонтальная компонента скорости
    private int speed;

    public int getSpeed() {
        return speed;
    }

    public void setSpeed(int speed) {
        this.speed = speed;
    }

    private double speedX;
    private double speedY;

    // Конструктор класса BouncingBall
    public BouncingBall(Field field){
        this.field=field;
        radius=new Double(Math.random()*(MAX_RADIUS-MIN_RADIUS)).intValue()+MIN_RADIUS;
        speed =new Double(Math.round(5*MAX_SPEED/radius)).intValue();
        if(speed>MAX_SPEED)
        {
            speed=MAX_SPEED;
        }
        // Начальное направление скорости тоже случайно,
// угол в пределах от 0 до 2PI
        double angle = Math.random()*2*Math.PI;
        speedX=3*Math.cos(angle);
        speedY=3*Math.sin(angle);

        color=new Color((float)Math.random(),(float)Math.random(),
                (float)Math.random());
        // Начальное положение мяча случайно
        x = Math.random()*(field.getSize().getWidth()-2*radius) + radius;
        y = Math.random()*(field.getSize().getHeight()-2*radius) + radius;
        // Создаѐм новый экземпляр потока, передавая аргументом
// ссылку на класс, реализующий Runnable (т.е. на себя)
        Thread thisThread=new Thread(this);
        // Запускаем поток
        thisThread.start();
    }

    // Метод run() исполняется внутри потока. Когда он завершает работу,
// то завершается и поток
    public void run() {
        try {
            while (true) {
// Синхронизация потоков на самом объекте поля
// Если движение разрешено - управление будет
// возвращено в метод
// В противном случае - активный поток заснѐт
                field.canMove(this);
                if (x + speedX <= radius) {
                    // Достигли левой стенки, отскакиваем право
                    Thread.sleep(1000);
                    speedX = -speedX;
                    x = radius;
                    field.magnetism(this);
                  // System.out.println("отскок лево");
                } else if (x + speedX >= field.getWidth() - radius) {
                    // Достигли правой стенки, отскок влево
                    Thread.sleep(1000);
                    speedX = -speedX;
                    x = new Double(field.getWidth() - radius).intValue();
                    field.magnetism(this);
                   // System.out.println("отскок право");
                } else if (y + speedY <= radius) {
// Достигли верхней стенки
                    Thread.sleep(1000);
                    speedY = -speedY;
                    y = radius;
                    field.magnetism(this);
                   // System.out.println("отскок вверх");

                } else if (y + speedY >= field.getHeight() - radius-100) {
// Достигли нижней стенки
                    Thread.sleep(1000);
                    speedY = -speedY;
                    y = new Double(field.getHeight() - radius-100).intValue();
                    field.magnetism(this);
                   // System.out.println("отскок вниз");
                } else {
// Просто смещаемся
                    x += speedX;
                    y += speedY;
                    // Засыпаем на X миллисекунд, где X определяется
// исходя из скорости
// Скорость = 1 (медленно), засыпаем на 15 мс.
// Скорость = 15 (быстро), засыпаем на 1 мс.
                    Thread.sleep(16 - speed);
                }
            }
        } catch (InterruptedException e) {
            e.printStackTrace();
        }
    }
// Метод прорисовки самого себя
    public void paint(Graphics2D canvas){
        canvas.setColor(color);
        canvas.setPaint(color);
        Ellipse2D.Double ball = new Ellipse2D.Double(x-radius,y-radius,2*radius,2*radius);
        canvas.draw((ball));
        canvas.fill(ball);
        }
    }


