% rooms

% id-capacity-equipment-availability
room(room1, 10, whiteboard,8,12).
room(room2, 20, projector,9,15).
room(room3, 30, smartboard,12,17).

% courses

% id-instructor-capacity-availability-room

course(course1, instructor1, 10, 9,10).
course(course2, instructor2, 20, 9,15).
course(course3, instructor3, 30, 12,17).

% instructors

% id-courses-equipment

instructor(instructor1, course1, whiteboard).
instructor(instructor2, course2, projector).
instructor(instructor3, course3, smartboard).

% students

% id-courses-handicapped

student(student1, [course1,course3], false).
student(student2, [course2,course3], true).
student(student3, [course3], false).




% Check which room can be assigned to a given class.
% check_room_class(+Room, +Course)
check_room_class(Room, Course) :-
    room(Room, Capacity1, _, Start1, End1),
    course(Course, _, Capacity2, Start2, End2),
    Start1 =< Start2,
    End1 >= End2,
    Capacity1 >= Capacity2,
    write('Room '), write(Room), write(' is available for course '), write(Course), nl.

% Check which room can be assigned to which classes
% check_rooms_class(+Course)
check_rooms_class(Course) :-
    course(Course, _, Capacity1, Start1, End1),
    room(Room, Capacity2, _, Start2, End2),
    Start2 =< Start1,
    End2 >= End1,
    Capacity2 >= Capacity1,
    write('Room '), write(Room), write(' is available for course '), write(Course), nl.

% Check whether a student can be enrolled to a given class.
% check_student_class(+Student, +Course)
check_student_class(Student, Course) :-
    student(Student, Courses, _),
    member(Course, Courses),
    write('Student '), write(Student), write(' can be enrolled to course '), write(Course), nl.

% Check which room a student can be assigned.
% check_students_class(+Student,+Room)
check_students_class(Student,Room) :-
    student(Student, _, Handicapped),
    room(Room, _, Equipment, _, _),
    Handicapped == true,
    Equipment == projector,   
    write('Student '), write(Student), write(' can be assigned to room '), write(Room), nl.
check_students_class(Student,Room) :-
    student(Student, _, Handicapped),
    room(Room, _, Equipment, _, _),
    Handicapped == true,
    Equipment == smartboard, 
    write('Student '), write(Student), write(' can be assigned to room '), write(Room), nl.
check_students_class(Student,Room) :-
    student(Student, _, Handicapped),
    room(Room, _, _, _, _),
    Handicapped == false,
    write('Student '), write(Student), write(' can be assigned to room '), write(Room), nl.


